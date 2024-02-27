local vim = vim
local unpack = table.unpack or unpack

local bit = require('bit')
--local function pcall(f, ...) return true, f(...) end -- debug

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
    upper = { match = vim.regex('^[[:upper:]]$'), cache = {} },
    lower = { match = vim.regex('^[[:lower:]]$'), cache = {} },
    digit = { match = vim.regex('^[[:digit:]]$'), cache = {} },
}

-- populate caches
for i = 0, 127 do
    local char = string.char(i)
    for _, v in pairs(patterns) do
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
    if #str == 0 then return {} end

    local result = {}
    local i = 1
    local byte = string.byte(str, i)
    if byte < 128 then
        while true do
            local nextByte -- next can be composing
            if i + 1 <= #str then
                nextByte = string.byte(str, i + 1)
                if nextByte >= 128 then break end
            end

            table.insert(result, string.char(byte))
            byte = nextByte
            i = i + 1
            if not nextByte then
                return result
            end
        end
    end

    local result2 = vim.fn.split(str:sub(i), '\\zs')
    vim.list_extend(result, result2)

    return result
end

--- functions for finding targets ---

-- v   v   v     v    v    v
-- wordWORDWord  word_WORD WORD ==
--                   ^          ^
local function test_split_identifiers(chars, cur_i)
    local cur_char = chars[cur_i]

    local is_match = false

    -- TODO: replace by category instead of 3 individual tests
    local lo = patterns.lower
    local up = patterns.upper
    local digit = patterns.digit

    if test(cur_char, lo.match, lo.cache) then
        local prev_char = chars[cur_i - 1]
        is_match = not test(prev_char, up.match, up.cache)
            and not test(prev_char, lo.match, lo.cache)
    elseif test(cur_char, up.match, up.cache) then
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
                    table.insert(targets, { line = lnum, col = col, charI = i, char = cur })
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

local defaultCharNormalize
do
  local normCache = {}
  local chars = {}
  for i = 1, 64 do
    local ch = string.char(i)
    chars[ch] =  vim.regex('^[[='..ch..'=]]\\c$')
    normCache[ch] = ch
  end
  for i = 91, 127 do
    local ch = string.char(i)
    chars[ch] = vim.regex('^[[='..ch..'=]]\\c$')
    normCache[ch] = ch
  end


  for i = 0, 25 do -- A-Z => a-z
    normCache[string.char(65 + i)] = string.char(97 + i)
  end

  defaultCharNormalize = function(char)
      local v = normCache[char]
      if v then return v end

      for k, pattern in pairs(chars) do
          if pattern:match_str(char) then
              normCache[char] = k
              return k
          end
      end

      chars[char] = vim.regex('^[[='..char..'=]]\\c$')
      normCache[char] = char
      return char
  end
end

local defaultLabels = {
    's', 'j', 'k', 'd', 'l', 'f', 'c', 'n', 'i', 'e', 'w', 'r', 'o',
    'm', 'u', 'v', 'a', 'q', 'p', 'x', 'z', '/',
}
local defaultNormalizedLabels = defaultLabels

local defaultOptions = {
    -- must be all unique and 1 cell wide. #labels >= 2
    labels = defaultLabels,
    normalizedLabels = defaultNormalizedLabels,
    char_normalize = defaultCharNormalize,
    recover_key = nil --[[
      a char (string) that, when pressed after the jump,
      restarts the previous jump with the same labels and everything.
      Use 's' if this is your key for jumping.
      Can also use vim.api.nvim_replace_termcodes(*<C-smth>*, true, false, true)
    ]],
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

local function createOptions(opts)
    if opts == nil then return defaultOptions end
    local result = {}

    result.recover_key = opts.recover_key
    result.namespace = opts.namespace or defaultOptions.namespace
    result.char_normalize = opts.char_normalize or defaultOptions.char_normalize

    local l = opts.labels
    if l then -- TODO: also add normalized labels into options
        result.labels = vim.list_extend({}, l)
    else
        result.labels = defaultLabels
    end

    -- TODO: check if normalized targets are different
    if result.char_normalize == defaultOptions.char_normalize then
        result.normalizedLabels = defaultNormalizedLabels
    else
        result.normalizedLabels = {}
        for k, v in ipairs(result.labels) do
            result.normalizedLabels[k] = result.char_normalize(v)
        end
    end

    local t = opts.special_targets
    if t then
        result.special_targets = { unique = toBoolean(t.unique), first = toBoolean(t.first) }
    else
        result.special_targets = defaultOptions.special_targets
    end

    local hl = opts.highlight
    if hl then
      result.highlight = {}
      for k, v in pairs(defaultOptions.highlight) do
          result.highlight[k] = hl[k] or v
      end
    else
      result.highlight = defaultOptions.highlight
    end

    return result
end

local function applyDefaultHighlight(opts)
    local options = createOptions(opts)
    vim.api.nvim_set_hl(0, options.highlight.backdrop, { link = 'Comment' })

    vim.api.nvim_set_hl(0, options.highlight.typed_char, { fg = 'grey', sp = 'red', underline = true, bold = true })
    vim.api.nvim_set_hl(0, options.highlight.rest_char, { fg = 'grey', sp='grey', underline = true, bold = true })

    vim.api.nvim_set_hl(0, options.highlight.unique, { bg = 'white', fg = 'black', bold = true })

    vim.api.nvim_set_hl(0, options.highlight.target_first, {
        bg = 'white', fg = 'black', bold = true,
    })
    vim.api.nvim_set_hl(0, options.highlight.target_first_typed, {
        bg = 'white', fg = 'black', sp = 'red', underline = true, bold = true,
    })

    vim.api.nvim_set_hl(0, options.highlight.typed_label, { sp = 'red', underline = true, bold  =  true })
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

local function choose(cond, ifTrue, ifFalse)
    if cond then return ifTrue
    else return ifFalse end
end

local function displayLabel(target, options, stage)
    local hl = options.highlight
    local ns = options.namespace
    local is_special = options.special_targets
    local displayLabels = options.labels
    local l = target.label

    local virt_text
    if not l then -- if first target (may be special) (unique rendered separately)
        if is_special.unique and target.unique then
            virt_text = { { target.char, hl.unique } }
        elseif is_special.first then
            virt_text = { { target.char, choose(stage == 0, hl.target_first, hl.target_first_typed) }}
        else
            virt_text = {
                { target.char, choose(stage == 0, hl.rest_char, hl.typed_char) },
                { target.char, hl.rest_label },
            }
        end
    elseif stage == 0 then
        virt_text = {
            { target.char, hl.rest_char },
            { displayLabels[l[2]]:rep(l[1]) .. displayLabels[l[3]], hl.rest_label },
        }
    elseif stage - 1 > l[1] then
        virt_text = {
            { target.char, hl.typed_char },
            { displayLabels[l[2]]:rep(l[1]) .. displayLabels[l[3]], hl.typed_label },
        }
    else
        virt_text = {
            { target.char, hl.typed_char },
            { displayLabels[l[2]]:rep(stage - 1), hl.typed_label },
            { displayLabels[l[2]]:rep(l[1] - stage + 1) .. displayLabels[l[3]], hl.rest_label },
        }
    end

    vim.api.nvim_buf_set_extmark(0, ns, target.line-1, target.col-1, {
      virt_text = virt_text, virt_text_pos = 'overlay', priority = 65535,
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
        return 1 + target.label[1] + 1
    end
end

--- functions for assigning labels to targets ---

--- index of first target >= position
local function findPosition(targets, line, col)
    local begin = 1
    local en = #targets + 1
    while begin < en do
        local m = begin + bit.rshift(en - begin, 1)
        local v = targets[m]
        if v.line < line or (v.line == line and v.col < col) then
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
local function sortTargets(targets, cursorLine, cursorCol, output)
end

-- more priority == more important
local function getKeyPriority(key)
    if key == ' ' or key == '\t' then
        return 0
    end

    local u = patterns.upper
    local l = patterns.lower
    local d = patterns.digit

    if test(key, u.match, u.cache) or test(key, l.match, l.cache) or test(key, d.match, d.cache) then
        return 2
    else
        return 1
    end
end

--- measurements ---

local Timer = {}
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

--- main function ---

local function collectTargets(options)
    --local timer = Timer:new()
    --timer:add('')

    local is_special = options.special_targets

    local winid = vim.api.nvim_get_current_win()
    local bufId = vim.api.nvim_get_current_buf()

    local wininfo = vim.fn.getwininfo(winid)[1]
    local topLine = wininfo.topline
    local botLine = wininfo.botline

    local cursorPos = vim.fn.getpos('.')
    local cursorLine, cursorCol = cursorPos[2], cursorPos[3]

    --timer:add('prep')

    -- find all tragets
    local wordStartTargets = get_targets(bufId, topLine, botLine)
    --timer:add('targets')

    if #wordStartTargets == 0 then
        vim.api.nvim_echo({{ 'no targets', 'ErrorMsg' }}, true, {})
        return
    end

    -- remove targets at cursor position
    do
        local startI = findPosition(wordStartTargets, cursorLine, cursorCol)
        local enI = startI
        while enI <= #wordStartTargets do
            local t = wordStartTargets[enI]
            if t.line == cursorLine and t.col == cursorCol then
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

    --timer:add('remove at cursor')

    local caseSensitive = toBoolean(options.case_sensitive)

    -- group targets by characters.
    -- Targets are kept in the same order
    local wordStartTargetsByChar = {}
    for _, target in ipairs(wordStartTargets) do
        local charN = options.char_normalize(target.char)
        if charN then
            local curData = wordStartTargetsByChar[charN]
            if not curData then
                curData = {}
                wordStartTargetsByChar[charN] = curData
            end
            table.insert(curData, target)
        else
            target.hidden = true
        end
    end

    --timer:add('by word')

    -- assign labels to groups of targets
    for charN, targets in pairs(wordStartTargetsByChar) do
        local priority = getKeyPriority(charN)
        assert(#targets > 0)
        if #targets == 1 then
            local target = targets[1]
            target.priority = priority -- maybe prioritize uniquie more if word chars?
            target.label = nil
            if is_special.unique then
                target.unique = true
            else
                targets.labelChars = options.normalizedLabels
            end
        else
            targets.labelChars = options.normalizedLabels

            -- either one may be outside range
            local nextI = findPosition(targets, cursorLine, cursorCol)
            local prevI = nextI - 1

            local beforeLineI, afterLineI

            local first
            if nextI <= #targets then
                first = targets[nextI]
                nextI = nextI + 1
                afterLineI = first.line
            end
            if prevI >= 1 then
                local curT = targets[prevI]
                if not first then
                    first = curT
                    prevI = prevI - 1
                end
                beforeLineI = curTline
            end
            assert(first ~= nil)

            -- first target is special, its label is the key itself (normalized)
            first.label = nil
            first.priority = priority

            local labelCharsI = {}
            for i, v in ipairs(options.normalizedLabels) do
                if charN ~= v then table.insert(labelCharsI, i) end
            end
            local labels = computeLabels(labelCharsI, #targets - 1)
            local labelsI = 1

            -- sort by lines
            -- note: can be unbalanced
            local labelsI = 1
            while true do
                local continue = false
                while nextI <= #targets do
                    continue = true
                    local curT = targets[nextI]
                    if curT.line == afterLineI then
                        curT.priority = priority
                        curT.label = labels[labelsI]
                        labelsI = labelsI + 1
                        nextI = nextI + 1
                    else
                        afterLineI = curT.line
                        break
                    end
                end

                while prevI >= 1 do
                    continue = true
                    local curT = targets[prevI]
                    if curT.line == beforeLineI then
                        curT.priority = priority
                        curT.label = labels[labelsI]
                        labelsI = labelsI + 1
                        prevI = prevI - 1
                    else
                        beforeLineI = curT.line
                        break
                    end
                end

                if not continue then break end
            end
        end
    end

    --timer:add('labels')

    -- remove targets with intersecting labels
    -- TODO: reassign labels? (should all be shorter than before)
    if #wordStartTargets > 1 then
        -- Go through the visible targets once for each priority
        -- (starting from highest) and remove targets at that priority
        -- if they intersect with other targets with priority >= cur.

        local prev
        -- Compute end bound (inclusive). Assumes all chars are length 1
        for i, t in ipairs(wordStartTargets) do
            local start = t.charI
            t.charEndI = t.charI + targetLabelLen(t, options) - 1

            -- don't show labels on leading whitespace yet (ugly)
            if t.priority == 0 and t.charI == 1 then t.hidden = true
            elseif not t.hidden and t.priority == 2 then
                -- remove intersecting at highest stage (since we're iterating them anyway)
                if prev and t.line == prev.line and t.charI <= prev.charEndI then
                    if prev.charEndI > cur.charEndI then
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
                    if cur.line == prev.line and cur.charI <= prev.charEndI then
                        -- it's better to keep prev if priorities are equal and it stops at same column?
                        if prev.priority < cur.priority or prev.charEndI > cur.charEndI then
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

    --timer:add('remove overlap')
    --timer:print()
    --print(#wordStartTargets)

    return {
      targets = wordStartTargets,
      targetsByChar = wordStartTargetsByChar,
      win = winid, buf = bufId,
      topLine = topLine, botLine = botLine,
    }
end

local function jumpToWord(options, targetsInfo)
    local winid = targetsInfo.win
    local bufId = targetsInfo.buf

    local topLine = targetsInfo.topLine
    local botLine = targetsInfo.botLine

    local hl = options.highlight
    local ns = options.namespace

    local wordStartTargets = targetsInfo.targets
    local wordStartTargetsByChar = targetsInfo.targetsByChar

    local topStart = { topLine-1, 0 }
    local botEnd = { botLine, -1 }
    local bgOptions = { priority = 65534 }
    local ns = options.namespace
    local backdrop = options.highlight.backdrop
    local function applyBg()
        -- For some reason, topLine and botLine are faster than 0 and last line
         --vim.highlight.range(bufId, ns, backdrop, { 0, 0 }, { vim.api.nvim_buf_line_count(bufId), -1 }, { }).
        -- Also, botline doesn't include partially shown lines. So no ' - 1'.
        -- Note: doesn't work with hlsearch, the pattern is still above :/
        -- neovim/neovim#22361 might be related
        vim.highlight.range(bufId, ns, backdrop, topStart, botEnd, bgOptions)
    end

    applyBg()

    --local t = Timer:new()
    --t:add('')

    for _, target in ipairs(wordStartTargets) do
        if not target.hidden then
            displayLabel(target, options, 0)
        end
    end
    --t:add('targets')
    --t:print()

    vim.cmd.redraw()
    local inputChar = get_input()
    if inputChar == nil then return end

    -- find group of targets that matches input characters
    local curTargetsChar = options.char_normalize(inputChar)
    local curTargets, curLabelChars
    for targetsChar, targets in pairs(wordStartTargetsByChar) do
        if targetsChar == curTargetsChar then
            curTargets = {}
            for _, target in ipairs(targets) do
                table.insert(curTargets, target)
            end
            curLabelChars = targets.labelChars
            break
        end
    end

    if not curTargets or #curTargets == 0 then
        vim.api.nvim_echo({{ 'no targets', 'ErrorMsg' }}, true, {})
        return
    end

    -- note: only if target was unique before (not if it became unique after smart case
    -- since that would be unexpected)
    if options.special_targets.unique and curTargets[1].unique then
        vim.fn.setpos('.', { 0, target.line, target.col, 0, target.charI })
        return
    end

    -- Recalculate which targets are hidden.
    -- Note: we do this only once, and not at every iteration
    -- since there is no point in showing additional targets after
    -- the first label char was typed.
    local lastVisibleI = 1
    for i = 2, #curTargets do
        local prev = curTargets[lastVisibleI]
        local cur = curTargets[i]
        if cur.line == prev.line and cur.charI <= prev.charEndI then
            if not cur.hidden or (prev.hidden and prev.charEndI > cur.charEndI) then
                curTargets[lastVisibleI] = cur
            end
        else
            lastVisibleI = lastVisibleI + 1
            curTargets[lastVisibleI] = cur
        end
    end

    -- Don't remove garbage from the end of the array, just track where the end is.
    local lastCurTarget = lastVisibleI

    -- find the terget to jump to, jump to that target
    local iteration = 1
    while true do
        vim.api.nvim_buf_clear_namespace(0, ns, 0, -1)
        applyBg()

        for i = 1, lastVisibleI do
          displayLabel(curTargets[i], options, iteration)
        end

        vim.cmd.redraw()
        inputChar = get_input()
        if inputChar == nil then break end
        local inputN = options.char_normalize(inputChar)

        local lastNewTarget = 0
        local found

        for i = 1, lastCurTarget do
            local target = curTargets[i]
            if not target.label then
                if inputN == curTargetsChar then
                    found = target
                    break
                end
            elseif target.label[1] >= iteration then
                if inputN == curLabelChars[target.label[2]] then
                    lastNewTarget = lastNewTarget + 1
                    curTargets[lastNewTarget] = target
                end
            elseif inputN == curLabelChars[target.label[3]] then
                found = target
                break
            end
        end

        if found then
            vim.fn.setpos('.', { 0, found.line, found.col, 0, found.charI })
            return
        end
        if lastNewTarget == 0 then
            vim.api.nvim_echo({{ 'no target', 'ErrorMsg' }}, true, {})
            return
        end

        lastCurTarget = lastNewTarget
        iteration = iteration + 1
    end
end

local function handleJump(params)
  local options = params[1]
  local targets = params[2]

  local ok, result = pcall(jumpToWord, options, targets)
  vim.api.nvim_buf_clear_namespace(0, options.namespace, 0, -1)
  vim.cmd[=[redraw!]=]

  if not ok then
    vim.api.nvim_echo({{'Error: '..vim.inspect(result), 'ErrorMsg'}}, true, {})
    return
  end

  if options.recover_key then return true end
end

-- create unique global mapping without having globals.
-- NOTE: at least one function must be present
local mapping = vim.api.nvim_exec2(
  "function s:a()\nendfun\nechon expand('<SID>')",
  { output = true }
).output

-- See https://github.com/neovim/neovim/issues/20793
-- TLDR: Displayed cursor is not updated if getcharstr()
-- is called immediatley after setting the cursor.
-- My workaround is to put getcharstr() in a mapping and execute it instead,
-- so that the cursor has time to update before the function is used.

local modes = { 'n', 'i', 'x', 's', 'c', 'o', 't', 'l' } -- are these all modes?
-- 'v' = 'x' + 's'

local recoverKeyI = 0

local function feedRecoverKeyStep(params)
  local name = mapping .. recoverKeyI
  recoverKeyI = recoverKeyI + 1

  vim.keymap.set(modes, name, function()
    for _, mode in ipairs(modes) do vim.api.nvim_del_keymap(mode, name) end

    local options = params[1]

    local typed = ''
    local key = options.recover_key

    while true do
      local ok, ch = pcall(vim.fn.getcharstr)
      if not ok then return end

      if string.sub(key, 1 + #typed, 1 + #typed + #ch-1) ~= ch then
        vim.api.nvim_feedkeys(typed..ch, '', false)
        return
      end
      if #key == #typed + #ch then break end
      typed = typed..ch
    end

    local ok, res = pcall(handleJump, params)
    if not ok then
      vim.api.nvim_echo({{'Error: '..vim.inspect(res), 'ErrorMsg'}}, true, {})
    elseif res then
      feedRecoverKeyStep(params)
    end
  end)

  -- I'm pretty sure this can be broken.
  -- I encountered some issues with reading text from nvim_feedkeys() of other plugins
  -- when doing 'getcharstr()' in a mapping in Ctrl_O in insert mode  o_o
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(name, true, false, true), '', false)
end

local function jump(opts)
    local options = createOptions(opts)

    local ok, result = pcall(collectTargets, options)
    if not ok then
      vim.api.nvim_echo({{'Error: '..vim.inspect(result), 'ErrorMsg'}}, true, {})
      return
    elseif result == nil then
      return
    end

    local params = { options, result }
    if handleJump(params) == true then
      feedRecoverKeyStep(params)
    end
end

applyDefaultHighlight()

--vim.keymap.set('n', 's', jump)

return {
    options = defaultOptions,
    apply_default_highlight = applyDefaultHighlight,
    jump = jump,
    regexp_test = test,
}
