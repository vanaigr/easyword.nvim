local vim = vim
local unpack = table.unpack or unpack

local bit = require('bit')

local function replace_keycodes(s)
    return vim.api.nvim_replace_termcodes(s, true, false, true)
end

local esc = replace_keycodes("<esc>")
local function get_input()
    local ok, ch = pcall(vim.fn.getcharstr)
    if ok and ch ~= esc then return ch
    else return nil end
end

local function toBoolean(value)
    if value then return true else return false end
end

--- patterns and functions for testing if a character should be considered a target ---

local patterns = {
    upper = { match = vim.regex('[[:upper:]]'), cache = {} },
    lower = { match = vim.regex('[[:lower:]]'), cache = {} },
    digit = { match = vim.regex('[[:digit:]]'), cache = {} },
    word  = { match = vim.regex('[[:upper:][:lower:][:digit:]]'), cache = {} },
}

local equivalenceCache = { {}, {} } -- { case insensitive, case sensitive }

-- populate caches
for _, v in pairs(patterns) do
    for i = 0, 127 do
        local char = string.char(i)
        v.cache[char] = v.match:match_str(char) ~= nil
    end
end

local function test(char, match, matchCache)
    if char == nil then return false end

    local value = matchCache[char]
    if value == nil then
        value = match:match_str(char) ~= nil
        matchCache[char] = value
    end
    return value
end

-- Ideally should match one character that can be typed.
-- Currently matches a single (possibly multibyte)
-- character without combining chars
local inputChar = vim.regex('.')

-- returns true if if str can be inputed by user
-- (if get_input() can return str)
function canBeInputed(str)
  local byte = string.byte(str, 1)
  if byte < 128 then --ascii
    return #str == 1
  else
    local start, en = inputChar:match_str(str)
    return start == 0 and en == #str
  end
end

local function splitByChars(str)
    local result = {}
    local i = 1

    -- why are regular expressions so slow
    -- why isn't there a faster version of a function
    -- that splits a string into characters ?????
    while i <= #str do
        local byte = string.byte(str, i)
        if byte < 128 then -- ascii
           table.insert(result, string.char(byte))
            i = i + 1
        else
            break
        end
    end

    if i <= #str then
        local result2 = vim.fn.split(str:sub(i), '\\zs')
        vim.list_extend(result, result2)
    end

    return result
end

local function makeCharMatch(char, caseSensitive)
  -- I hope this wouldn't need to be escaped
  -- since I have no idea how to escape it
  if caseSensitive then 
    return vim.regex('[[='..char..'=]]')
  else 
    return vim.regex('[[='..char..'=]]\\c')
  end
end

local function eqClassCache(char, caseSensitive)
    local cacheType = equivalenceCache[caseSensitive and 2 or 1]
    local cache = cacheType[char]
    if not cache then
      cache = {}
      cacheType[char] = cache
    end
    return cache
end

--- functions for finding targets ---

-- v   v   v     v    v    v    
-- wordWORDWord  word_WORD WORD ==
--                   ^          ^
local function test_split_identifiers(chars, cur_i)
    local cur_char = chars[cur_i]

    local is_match = false

    local up = patterns.upper
    local lo = patterns.lower
    local digit = patterns.digit
    local word = patterns.word

    if test(cur_char, up.match, up.cache) then
        local prev_char = chars[cur_i - 1]
        if not test(prev_char, up.match, up.cache) then
            is_match = true
        else
            local next_char = chars[cur_i + 1]
            is_match = test(next_char, lo.match, lo.cache)
                or test(next_char, digit.match, digit.cache)
        end
    elseif test(cur_char, digit.match, digit.cache) then
        is_match = not test(chars[cur_i-1], digit.match, digit.cache)
    elseif test(cur_char, lo.match, lo.cache) then
        local prev_char = chars[cur_i - 1]
        is_match = test(prev_char, digit.match, digit.cache)
            or not test(prev_char, word.match, word.cache)
    else
        local prev_char = chars[cur_i - 1]
        is_match = prev_char ~= cur_char -- matching only first character in ==, [[ and ]]
    end

    return is_match
end

local function get_targets(bufId, topLine, botLine)
    local lnum = topLine

    local targets = {}
    while lnum <= botLine do
        local fold_end = vim.fn.foldclosedend(lnum)
        if fold_end ~= -1 then
            lnum = fold_end + 1
        else
            local line = vim.api.nvim_buf_get_lines(bufId, lnum-1, lnum, true)[1]
            local chars = splitByChars(line)

            -- if I could just use utf_ptr2CharInfo() ...
            local col = 1
            for i, cur in ipairs(chars) do
                if test_split_identifiers(chars, i) then
                    -- charI is for curswant and label intersection removal.
                    -- The fact that curswant is 1-idexed and measured in characters 
                    -- and not bytes or screen cells is a secret (shh, don't tell anyone)
                    table.insert(targets, { char = cur, pos = { lnum, col, charI = i } })
                end
                col = col + string.len(cur)
            end
            assert(string.len(line) == col - 1)

            lnum = lnum + 1
        end
    end
    return targets
end

--- options ---

local defaultOptions = {
    case_sensitive = false,
    smart_case = true,
    labels = { -- must be all unique and 1 cell wide
        's', 'j', 'k', 'd', 'l', 'f', 'c', 'n', 'i', 'e', 'w', 'r', 'o',
        'm', 'u', 'v', 'a', 'q', 'p', 'x', 'z', '/',
    },
    special_targets = {
        unique = false, -- treat unique targets specially (don't assign labels)
        first = true, -- true = don't display target char in label twice, use special highlight
    },
    highlight = {
        backdrop = 'EasywordBackdrop',
        unique = 'EasywordUnique',
        target_first = 'EasywordTargetFirst',
        target_first_typed = 'EasywordTargetFirstTyped',
        typed_char = 'EasywordTypedChar',
        rest_char = 'EasywordRestChar',
        typed_label = 'EasywordTypedLabel',
        rest_label = 'EasywordRestLabel',
    },
    namespace = vim.api.nvim_create_namespace('Easyword'),
}
function defaultOptions:get_typed_labels(char)
  return self.labels -- must be same length as self.labels
end

local function createOptions(opts)
    if opts == nil then return defaultOptions
    else return vim.tbl_deep_extend('keep', opts, defaultOptions) end
end

local function applyDefaultHighlight(opts)
    local options = createOptions(opts)
    vim.api.nvim_set_hl(0, options.highlight.backdrop, { link = 'Comment' })

    vim.api.nvim_set_hl(0, options.highlight.typed_char, { sp='red', underline=true, bold = true })
    vim.api.nvim_set_hl(0, options.highlight.rest_char, { bg = 'black', fg = 'grey', bold = true })

    vim.api.nvim_set_hl(0, options.highlight.unique, { bg = 'white', fg = 'black', bold = true })

    vim.api.nvim_set_hl(0, options.highlight.target_first, {
        bg = 'white', fg = 'black', bold = true,
    })
    vim.api.nvim_set_hl(0, options.highlight.target_first_typed, {
        bg = 'white', fg = 'black', sp = 'red', underline = true, bold = true,
    })

    vim.api.nvim_set_hl(0, options.highlight.typed_label, { sp='red', underline=true, bold = true })
    vim.api.nvim_set_hl(0, options.highlight.rest_label, { bg = 'black', fg = 'white', bold = true })
end

--- labels ---

-- generate variable length labels that use at most 2 characters, second char is always used only once at the end
-- returns { { repCount, repCharI, lastCharI }, ... }
local function computeLabels(labelCharsI, max)
  if #labelCharsI < 2 then
    if max == 0 then return {} end
    if #labelCharsI == 1 and max == 1 then return { 0, labelCharsI[1], labelCharsI[1] } end
    error('could not generate ' .. max .. ' labels from ' .. #labelCharsI .. ' label variations')
  end

  -- list of #chars in labels that have repeating label
  local sameCharLabels = {}
  for _, i in ipairs(labelCharsI) do table.insert(sameCharLabels, { 0, i, i }) end

  local labels = {}

  local curI = 1
  while #labels + #sameCharLabels < max do
    local curLabel = sameCharLabels[curI]
    local labelChar = curLabel[2]
    local curLabelLen = curLabel[1] + 1

    for _, i in ipairs(labelCharsI) do
      if i ~= labelChar then
        table.insert(labels, { curLabelLen, labelChar, i })
      end
    end

    curLabel[1] = curLabel[1] + 1
    curI = curI + 1
    if curI > #sameCharLabels then curI = 1 end
  end

  -- merge same char labels and regular labels
  local addedCount = 0
  local labelsI = 1
  while addedCount < #sameCharLabels do
    local sameCharLabel = sameCharLabels[curI]
    local label = labels[labelsI]

    if not label then
      table.insert(labels, sameCharLabel)
      addedCount = addedCount + 1
      curI = curI + 1
      if curI > #sameCharLabels then curI = 1 end
    else
      if sameCharLabel[1] <= label[1] then
        table.insert(labels, labelsI, sameCharLabel)
        addedCount = addedCount + 1
        curI = curI + 1
        if curI > #sameCharLabels then curI = 1 end
      end
      labelsI = labelsI + 1
    end
  end

  return labels
end

local function labelString(displayLabels, label)
    local s = ''
    if label[1] > 0 then
        s = displayLabels[label[2]]:rep(label[1])
    end
    if label[3] then
        s = s .. displayLabels[label[3]]
    end
    return s
end

-- Ideally should take into account display labels size
-- and position on screen (for tabs).
-- Assumes all chars are 1 cell wide
local function labelLength(label)
    local len = 0
    if label[1] > 0 then len = label[1] end
    if label[3] then len = len + 1 end
    return len
end

local function displayLabel(target, options, firstTyped)
    local hl = options.highlight
    local ns = options.namespace
    local is_special = options.special_targets
    local displayLabels = options.labels

    local virt_text
    if not target.label then -- if first target (may be special) (unique rendered separately)
        if is_special.unique and target.unique then
            virt_text = { { target.char, hl.unique } }
        elseif is_special.first then
            if firstTyped then
                virt_text = { { target.char, hl.target_first_typed } }
            else
                virt_text = { { target.char, hl.target_first } }
            end
        else
            if firstTyped then
                virt_text = {
                    { target.char, hl.typed_char },
                    { target.char, hl.rest_label },
                }
            else
                virt_text = {
                    { target.char, hl.rest_char },
                    { target.char, hl.rest_label },
                }
            end
        end
    else
        if firstTyped then
            virt_text = {
                { target.char, hl.typed_char },
                { labelString(displayLabels, target.typedLabel), hl.typed_label },
                { labelString(displayLabels, target.label), hl.rest_label },
            }
        else
            virt_text = {
                { target.char, hl.rest_char },
                { labelString(displayLabels, target.label), hl.rest_label },
            }
        end
    end

    vim.api.nvim_buf_set_extmark(0, ns, target.pos[1]-1, target.pos[2]-1, {
        virt_text = virt_text,
        virt_text_pos = 'overlay',
        hl_mode = 'combine',
    })
end

-- Ideally should take into account display labels size
-- and position on screen (for tabs).
-- Assumes all chars are 1 cell wide
local function targetLabelLen(target, options)
    local is_special = options.special_targets

    if not target.label then
        if is_special.unique and target.unique then
            return 1
        elseif is_special.first then
            return 1
        else
            return 2
        end
    else
        return 1 + labelLength(target.typedLabel) + labelLength(target.label)
    end
end

--- functions for assigning labels to targets ---

--- index of first target >= position
local function findPosition(targets, position)
    local begin = 1
    local en = #targets + 1
    while begin < en do
        local m = begin + bit.rshift(en - begin, 1)
        local v = targets[m]
        if v.pos[1] < position[1] or (v.pos[1] == position[1] and v.pos[2] < position[2]) then
            begin = m + 1
        else
            en = m
        end
    end
    return begin
end

--- sort labels relative to cursor:
--- interleave lines with targets above and below cursor, reverse order on line for lines above.
--- First mark will always be next after cursor, second is previous before the cursor.
--- Input must be sorted by lines and then columns (increasing)
local function sortTargets(cursorPos, targets)
    -- may be outside range
    local nextI = findPosition(targets, cursorPos)
    local prevI = nextI - 1

    local sortedTargets = {}

    local afterLineI
    local beforeLineI

    -- add first labels
    if nextI <= #targets then
        local curT = targets[nextI]
        nextI = nextI + 1
        table.insert(sortedTargets, curT)
        afterLineI = curT.pos[1]
    end

    if prevI >= 1 then
        local curT = targets[prevI]
        prevI = prevI - 1
        table.insert(sortedTargets, curT)
        beforeLineI = curT.pos[1]
    end

    -- sort by lines
    -- note: can be unbalanced
    while true do
        local continue = false
        while nextI <= #targets do
            continue = true
            local curT = targets[nextI]
            if curT.pos[1] == afterLineI then
                table.insert(sortedTargets, curT)
                nextI = nextI + 1
            else
                afterLineI = curT.pos[1]
                break
            end
        end

        while prevI >= 1 do
            continue = true
            local curT = targets[prevI]
            if curT.pos[1] == beforeLineI then
                table.insert(sortedTargets, curT)
                prevI = prevI - 1
            else
                beforeLineI = curT.pos[1]
                break
            end
        end

        if not continue then break end
    end

    return sortedTargets
end

local function setFirstTargetLabel(target, caseSensitive)
    target.label = nil
    target.match = makeCharMatch(target.char, caseSensitive)
    target.matchCache = eqClassCache(target.char, caseSensitive)
end

local function setUniqueTargetLabel(target)
  target.label = nil
  target.unique = true
end

-- more priority == more important
local function getKeyPriority(key)
    if key == ' ' or key == '\t' then
        return 0
    elseif test(key, patterns.word.match, patterns.word.cache) then
        return 2
    else
        return 1
    end
end

--- measurements ---

local Timer = {}

if false then
  Timer.__index = Timer
  function Timer:new()
    local o = {}
    setmetatable(o, Timer)
    return o
  end
  function Timer:add(name)
    table.insert(self, { vim.loop.hrtime(), name })
  end
  function Timer:print()
    local prev
    print(' ')
    for _, data in ipairs(self) do
      if prev then
        print(data[2], vim.fn.round((data[1] - prev[1]) / 1000) / 1000)
      end
      prev = data
    end
  end
else
  Timer.__index = Timer
  function Timer:new()
    local o = {}
    setmetatable(o, Timer)
    return o
  end
  function Timer:add(name) end
  function Timer:print() end
end

--- main function ---

local function jumpToWord(options)
    local timer = Timer:new()
    timer:add('')

    local hl = options.highlight
    local ns = options.namespace
    local displayLabels = options.labels
    local is_special = options.special_targets

    local winid = vim.api.nvim_get_current_win()
    local bufId = vim.api.nvim_win_get_buf(winid)

    local wininfo = vim.fn.getwininfo(winid)[1]
    local topLine = wininfo.topline
    local botLine = wininfo.botline

    local function clear()
        vim.api.nvim_buf_clear_namespace(0, ns, 0, -1)
        -- For some reason, topLine and botLine are faster than 0 and last line
        -- vim.highlight.range(bufId, ns, hl.backdrop, { 0, 0 }, { vim.api.nvim_buf_line_count(bufId), -1 }, { }).
        -- Also, botline doesn't include partially shown lines. So no ' - 1'.
        vim.highlight.range(bufId, ns, hl.backdrop, { topLine-1, 0 }, { botLine, -1 }, { })
    end

    local cursorPos = vim.fn.getpos('.')
    cursorPos = { cursorPos[2], cursorPos[3] }

    timer:add('prep')

    -- find all tragets 
    local wordStartTargets = get_targets(bufId, topLine, botLine)
    timer:add('targets')

    if #wordStartTargets == 0 then
        vim.api.nvim_echo({{ 'no targets', 'ErrorMsg' }}, true, {})
        return
    end

    -- remove targets at cursor position
    do
        local startI = findPosition(wordStartTargets, cursorPos)
        local enI = startI
        while enI <= #wordStartTargets do
            local t = wordStartTargets[enI]
            if t.pos[1] == cursorPos[1] and t.pos[2] == cursorPos[2] then
                enI = enI + 1
            else
                break
            end
        end

        local count = enI - startI
        if count ~= 0 then
            for i = startI, #wordStartTargets - count do
                wordStartTargets[i] = wordStartTargets[i + count]
            end
            for i = #wordStartTargets - count + 1, #wordStartTargets do
                wordStartTargets[i] = nil
            end
        end
    end

    timer:add('remove dup')

    local caseSensitive = toBoolean(options.case_sensitive)

    -- group targets by characters.
    -- Targets are kept in the same order
    local wordStartTargetsByChar = {}
    for _, target in ipairs(wordStartTargets) do
        local tChar = target.char

        local curData = wordStartTargetsByChar[tChar]
        if not curData then
            local match = makeCharMatch(tChar, caseSensitive)
            local cache = eqClassCache(tChar, caseSensitive)

            for char, data in pairs(wordStartTargetsByChar) do
                if test(char, match, cache) then
                    curData = data
                    break
                end
            end
        end
        if not curData then
            curData = {}
            wordStartTargetsByChar[tChar] = curData
        end
        table.insert(curData, target)
    end

    timer:add('by word')

    clear()

    timer:add('clear hl')

    -- assign labels to groups of targets and display them
    for key, targets in pairs(wordStartTargetsByChar) do
        local priority = getKeyPriority(key)
        if #targets == 1 then
            local target = targets[1]
            target.priority = priority -- maybe prioritize uniquie more if word chars?
            if is_special.unique then
                setUniqueTargetLabel(target)
            else
                local labels = options:get_typed_labels(key)
                targets.labelChars = labels
                setFirstTargetLabel(target)
            end
        else
            local labelChars = options:get_typed_labels(key)
            targets.labelChars = labelChars

            local sortedTargets = sortTargets(cursorPos, targets)
            local labelCharsI = {}

            -- first target is special, its label is the key itself
            local first = sortedTargets[1]
            local targetStart = 1
            if first then
                targetStart = 2
                setFirstTargetLabel(first)
                first.priority = priority
                local charMatch = first.match
                local matchCache = first.matchCache

                -- we need to remove all other label chars that that are quivalent.
                -- Case sensitivity is the same as for input characters, as it gives more
                -- available label chars if case sensitivity is turned on.
                for i = 1, #labelChars do
                    if not test(labelChars[i], charMatch, matchCache) then
                        table.insert(labelCharsI, i)
                    end
                end
            else
                for i = 1, #labelChars do
                    table.insert(labelCharsI, i)
                end
            end

            local labels = computeLabels(labelCharsI, #targets - targetStart + 1)
            for i = 1, #targets - targetStart + 1 do
                local target = sortedTargets[targetStart + i - 1]
                target.priority = priority
                target.label = labels[i]
                target.typedLabel = { 0, labels[i][2] }
            end
        end
    end

    timer:add('labels')

    -- remove targets with intersecting labels
    -- TODO: reassign labels? (should all be shorter than before)
    if #wordStartTargets > 1 then
        -- Go through the visible targets once for each priority
        -- (starting from highest) and remove targets at that priority
        -- if they intersect with other targets with priority >= cur.

        local prev
        -- Compute end bound (inclusive). Assumes all chars are length 1
        for i, t in ipairs(wordStartTargets) do
            local start = t.pos.charI
            t.pos.charEndI = t.pos.charI + targetLabelLen(t, options) - 1

            -- don't show labels on leading whitespace yet (ugly)
            if t.priority == 0 and t.pos.charI == 1 then t.hidden = true
            elseif t.priority == 2 then -- and t is not hidden
                -- remove intersecting at highest stage (since we're iterating them anyway)
                if prev and t.pos[1] == prev.pos[1] and t.pos.charI <= prev.pos.charEndI then
                    if prev.pos.charEndI > cur.pos.charEndI then
                        prev.hidden = true
                        prev = cur
                    else
                        cur.hidden = the
                    end
                else
                    prev = cur
                end
            end
        end

        -- hide intersecting targets
        for stage = 1, 0, -1 do
            -- find starting target (visible, priority >= current)
            local prevI = 1
            local prev = wordStartTargets[prevI]
            while prev and (prev.hidden or prev.priority < stage) do
                prevI = prevI + 1
                prev = wordStartTargets[prevI]
            end

            local i = prevI + 1
            while i <= #wordStartTargets do
                local cur = wordStartTargets[i]
                if not cur.hidden and cur.priority >= stage then
                    if cur.pos[1] == prev.pos[1] and cur.pos.charI <= prev.pos.charEndI then
                        -- it's better to keep prev if priorities are equal and it stops at same column?
                        if prev.priority < cur.priority or prev.pos.charEndI > cur.pos.charEndI then
                            prev.hidden = true
                            prev = cur
                        else
                            cur.hidden = true
                        end
                    else
                        prev = cur
                    end
                end
                i = i + 1
            end
        end
    end

    timer:add('remove overlap')

    for _, target in ipairs(wordStartTargets) do
        if not target.hidden then
            displayLabel(target, options, false)
        end
    end

    timer:add('end')

    timer:print()

    vim.cmd.redraw()
    local inputChar = get_input()
    if inputChar == nil then return end

    -- apply smart case
    -- NOTE: we can only add restrictions, since we already computed 
    -- the labels, and if we would have to join 2+ targets groups, 
    -- the labels would collide; and label chars also wouldn't be the same 
    -- (would break first target, which assumes other targets don't take its character)
    local sensitivityChanged = false
    if not caseSensitive and options.smart_case then
        local upperChar = vim.fn.toupper(inputChar)
        local lowerChar = vim.fn.tolower(inputChar)

        if upperChar ~= lowerChar and inputChar == upperChar
          and canBeInputed(upperChar) and canBeInputed(lowerChar) 
        then
          sensitivityChanged = true
          caseSensitive = true
        end
    end

    local inputMatch = makeCharMatch(inputChar, caseSensitive)
    local matchCache = eqClassCache(inputChar, caseSensitive)

    -- find group of targets that matches input characters
    local curTargets

    if sensitivityChanged then
        -- sensitivityChanged => prev was case insensitive
        local oldInputMatch = makeCharMatch(inputChar, false)
        local oldInputCache = eqClassCache(inputChar, false)

        -- find targets that would've been used and filter ones that have different case
        local newTargets = {}
        for targetsChar, targets in pairs(wordStartTargetsByChar) do
            if test(targetsChar, oldInputMatch, oldInputCache) then
                for _, target in ipairs(targets) do
                    if test(target.char, inputMatch, matchCache) then
                        table.insert(newTargets, target)
                    end
                end
                newTargets.labelChars = targets.labelChars
                break
            end
        end

        if #newTargets ~= 0 then
          curTargets = newTargets
        end
    else
        for targetsChar, targets in pairs(wordStartTargetsByChar) do
            if test(targetsChar, inputMatch, matchCache) then
                curTargets = targets
                break
            end
        end
    end

    if not curTargets or #curTargets == 0 then
        vim.api.nvim_echo({{ 'no targets', 'ErrorMsg' }}, true, {})
        return
    end

    local curLabelChars = curTargets.labelChars

    -- note: only if target was unique before (not if it became unique after smart case
    -- since that would be unexpected)
    if #curTargets == 1 and curTargets[1].unique and options.special_targets.unique then
        local pos = curTargets[1].pos
        vim.fn.setpos('.', { 0, pos[1], pos[2], 0, pos.charI })
        return
    end

    -- recalculate if targets are hidden.
    -- Note: we do this only once, and not at every iterations
    -- since there is no point in showing additional targets after
    -- the first label char was typed.
    local prev = curTargets[1]
    prev.hidden = false
    for i = 2, #curTargets do
        local cur = curTargets[i]
        cur.hidden = false
        if cur.pos[1] == prev.pos[1] and cur.pos.charI <= prev.pos.charEndI then
            if prev.pos.charEndI > cur.pos.charEndI then
                prev.hidden = true
                prev = cur
            else
                cur.hidden = true
            end
        else
            prev = cur
        end
    end

    do -- Remove hidden targets
        local i = 1

        -- skip visible from start
        while i <= #curTargets do
            local target = curTargets[i]
            if target.hidden then
                break
            end
            i = i + 1
        end
        local putI = i

        -- move visible targets to start
        while i <= #curTargets do
            local target = curTargets[i]
            if not target.hidden then
                curTargets[putI] = target
                putI = putI + 1
            end
            i = i + 1
        end

        -- remove targets past the end
        i = #curTargets
        while i >= putI do
            curTargets[i] = nil
            i = i - 1
        end
    end

    assert(#curTargets ~= 0)

    -- find the terget to jump to, jump to that target
    local i = 1
    while true do
        clear()
        for _, target in ipairs(curTargets) do
            displayLabel(target, options, true)
        end

        vim.cmd.redraw()
        inputChar = get_input()
        if inputChar == nil then break end

        local newTargets = {}
        local found

        for _, target in ipairs(curTargets) do
          if not target.label then
            if test(inputChar, target.match, target.matchCache) then
              found = target
              break
            end
          else
            if target.label[1] > 0 then
              if inputChar == curLabelChars[target.label[2]] then
                target.label[1] = target.label[1] - 1
                target.typedLabel[1] = target.typedLabel[1] + 1
                table.insert(newTargets, target)
              end
            else
              if inputChar == curLabelChars[target.label[3]] then
                found = target
                break
              end
            end
          end
        end

        if found then
            local pos = found.pos
            vim.fn.setpos('.', { 0, pos[1], pos[2], 0, pos.charI })
            return
        end
        if #newTargets == 0 then
            vim.api.nvim_echo({{ 'no target', 'ErrorMsg' }}, true, {})
            return
        end

        curTargets = newTargets
        i = i + 1
    end
end

local function jump(opts)
    local options = createOptions(opts)
    -- jumpToWord(options) -- if stacktrace is needed
    local ok, result = pcall(jumpToWord, options)
    if not ok then vim.api.nvim_echo({{'Error: '..vim.inspect(result), 'ErrorMsg'}}, true, {}) end
    vim.api.nvim_buf_clear_namespace(0, options.namespace, 0, -1)
end

applyDefaultHighlight()

--vim.keymap.set('n', 's', jump)

return {
    options = defaultOptions,
    apply_default_highlight = applyDefaultHighlight,
    jump = jump,
    regexp_test = test,
}
