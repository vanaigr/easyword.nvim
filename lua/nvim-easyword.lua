local vim = vim
local unpack = table.unpack or unpack

-- code is taken from https://github.com/VanaIgr/leap-by-word.nvim.git
-- (fork from https://github.com/Sleepful/leap-by-word.nvim)

local function replace_keycodes(s)
    return vim.api.nvim_replace_termcodes(s, true, false, true)
end

local esc = replace_keycodes("<esc>")
local function get_input()
    local ok, ch = pcall(vim.fn.getcharstr)
    if ok and ch ~= esc then return ch
    else return nil end
end

-- patterns and functions for testing if a character should be considered a target
local patterns = {
    upper = { match = [=[\v[[:upper:]]\C]=], cache = {} },
    lower = { match = [=[\v[[:lower:]]\C]=], cache = {} },
    digit = { match = [=[\v[[:digit:]]\C]=], cache = {} },
    word  = { match = [=[\v[[:upper:][:lower:][:digit:]]\C]=], cache = {} },
}

local equivalenceCache = { {}, {} } -- case insensitive, sensitive

do -- populate caches
    for _, v in pairs(patterns) do
        -- for some reason, vim gives 
        -- 'E5108: Error executing lua Vim:E976: using Blob as a String'
        -- for ANY function in vim.fn if string contains \0
        -- we know that neigher of pattrens patch \0, so just set it before
        v.cache[string.char(0)] = false

        for i = 1, 127 do
            local char = string.char(i)
            v.cache[char] = vim.fn.match(char, v.match) == 0
        end
    end
end

local function test(char, match, matchCache)
    -- vim.fn.match returns false for nil char, but not if pattern contains `[:lower:]`
    if char == nil then return false end

    local value = matchCache[char]
    if value == nil then
        value = vim.fn.match(char, match) == 0
        matchCache[char] = value
    end
    return value
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
        for _, v in ipairs(result2) do
            table.insert(result, v)
        end
    end

    return result
end

local function toBoolean(value)
    if value then return true else return false end
end

local function makeCharMatch(char, caseSensitive)
  if caseSensitive then 
    return '\\v[[='..char..'=]]\\C'
  else 
    return '\\v[[='..char..'=]]\\c'
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

local function get_targets(bufId, topLine, botLine, test_func)
    local lnum = topLine

    local targets = {}
    while lnum <= botLine do
        local fold_end = vim.fn.foldclosedend(lnum) -- winId?
        if fold_end ~= -1 then
            lnum = fold_end + 1
        else
            local line = vim.api.nvim_buf_get_lines(bufId, lnum-1, lnum, true)[1]
            local chars = splitByChars(line)

            local col = 1
            for i, cur in ipairs(chars) do -- search beyond last column
                if test_func(chars, i) then
                    table.insert(targets, { char = cur, pos = { lnum, col } })
                end
                col = col + string.len(cur)
            end
            assert(string.len(line) == col - 1)

            lnum = lnum + 1
        end
    end
    return targets
end

local defaultOptions = {
    case_sensitive = false,
    smart_case = true,
    labels = { -- don't use tab here
        's', 'j', 'k', 'd', 'l', 'f', 'c', 'n', 'i', 'e', 'w', 'r', 'o',
        'm', 'u', 'v', 'a', 'q', 'p', 'x', 'z', '/',
    },
    highlight = {
        backdrop = 'EasywordBackdrop',
        unique = 'EasywordUnique',
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
    vim.api.nvim_set_hl(0, options.highlight.unique, { bg = 'white', fg = 'black', bold = true })
    vim.api.nvim_set_hl(0, options.highlight.typed_char, { sp='red', underline=true, bold = true })
    vim.api.nvim_set_hl(0, options.highlight.rest_char, { bg = 'black', fg = 'grey', bold = true })
    vim.api.nvim_set_hl(0, options.highlight.typed_label, { sp='red', underline=true, bold = true })
    vim.api.nvim_set_hl(0, options.highlight.rest_label, { bg = 'black', fg = 'white', bold = true })
end

--- genetate variable length labels that use at most 2 characters, second char is always used only once at the end
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
    if not label then
      return ''
    elseif label.literal then
      return label.literal
    else
      local s = ''
      if label[1] > 0 then
        s = displayLabels[label[2]]:rep(label[1])
      end
      if label[3] then
        s = s .. displayLabels[label[3]]
      end
      return s
    end
end

--- index of first target >= position
local function findPosition(targets, position)
    local begin = 1
    local en = #targets + 1
    while begin < en do
        local m = begin + math.floor((en - begin) / 2)
        local v = targets[m]
        if v.pos[1] < position[1] or (v.pos[1] == position[1] and v.pos[2] < position[2]) then
            begin = m + 1
        else
            en = m
        end
    end
    return begin
end

--- sort labels (originally left -> right, top -> bottom) relative to cursor:
--- interleave lines with targets above and below cursor, reverse order on line for lines above.
--- First mark will always be next after cursor, second is previous before the cursor.
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

    --- sort by lines
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

local function assignTargetLabels(targets, cursorPos, labelChars, caseSensitive)
    targets = sortTargets(cursorPos, targets)
    local labelCharsI = {}

    -- first target is special, its labes is the key itself
    local first = targets[1]
    local targetStart = 1
    if first then
        targetStart = 2
        first.label = { literal = first.char }

        local charMatch = makeCharMatch(first.char, caseSensitive)
        local matchCache = eqClassCache(first.char, caseSensitive)
        first.match = charMatch
        first.matchCache = matchCache

        -- we need to remove all other label chars that that are quivalent
        -- case sensitivity is the same as for input characters, since it gives more
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
        local target = targets[targetStart + i - 1]
        target.label = labels[i]
        target.typedLabel = { 0, labels[i][2] }
    end

    return targets
end

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

local function jumpToWord(options)
    local timer = Timer:new()
    timer:add('')

    local hl = options.highlight
    local ns = options.namespace
    local displayLabels = options.labels

    local winid = vim.api.nvim_get_current_win()
    local bufId = vim.api.nvim_win_get_buf(winid)

    local wininfo = vim.fn.getwininfo(winid)[1]
    local topLine = wininfo.topline
    local botLine = wininfo.botline

    local function clear()
        vim.api.nvim_buf_clear_namespace(0, ns, 0, -1)
        -- For some reason, topLine and botLine are faster than 0 and last line
        -- vim.highlight.range(bufId, ns, hl.backdrop, { 0, 0 }, { vim.api.nvim_buf_line_count(bufId), -1 }, { }).
        -- Also, botline doesn't include partially shown lines. So no -1.
        vim.highlight.range(bufId, ns, hl.backdrop, { topLine-1, 0 }, { botLine, -1 }, { })
    end

    local cursorPos = vim.fn.getpos('.')
    cursorPos = { cursorPos[2], cursorPos[3] }

    timer:add('prep')

    -- find all word tragets 
    local wordStartTargets = get_targets(
        bufId, topLine, botLine, function(chars, i)
            return test(chars[i], patterns.word.match, patterns.word.cache)
                and test_split_identifiers(chars, i)
        end
    )

    timer:add('targets')

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

    -- group targets by characters
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
        if #targets == 1 then
            local target = targets[1]
            vim.api.nvim_buf_set_extmark(0, ns, target.pos[1]-1, target.pos[2]-1, {
                virt_text = { { target.char, hl.unique } },
                virt_text_pos = 'overlay',
                hl_mode = 'combine'
            })
        else
            local labels = options:get_typed_labels(key)
            targets = assignTargetLabels(targets, cursorPos, labels, caseSensitive)
            targets.labelChars = labels
            wordStartTargetsByChar[key] = targets
            for _, target in ipairs(targets) do
                vim.api.nvim_buf_set_extmark(0, ns, target.pos[1]-1, target.pos[2]-1, {
                    virt_text = {
                        { target.char, hl.rest_char },
                        { labelString(displayLabels, target.label), hl.rest_label },
                    },
                    virt_text_pos = 'overlay',
                    hl_mode = 'combine'
                })
            end
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
        local upperChar = splitByChars(vim.fn.toupper(inputChar))
        local lowerChar = splitByChars(vim.fn.tolower(inputChar))

        -- if both cases can be typed, are different, and current input character is uppper case
        if #upperChar == 1 and #lowerChar == 1 and upperChar[1] ~= lowerChar[1] and inputChar == upperChar[1] then
          sensitivityChanged = true
          caseSensitive = true
        end
    end

    local inputMatch = makeCharMatch(inputChar, caseSensitive)
    local matchCache = eqClassCache(inputChar, caseSensitive)

    -- find group of targets that matches input characters
    local curTargets
    local curLabelChars

    if sensitivityChanged then
        -- sensitivityChanged => prev was case insensitive
        local oldInputMatch = makeCharMatch(inputChar, false)
        local oldInputCache = eqClassCache(inputChar, false)

        -- find targets that would've been used and filter ones that have different case
        local newTargets = {}
        local newLabelChars
        for targetsChar, targets in pairs(wordStartTargetsByChar) do
            if test(targetsChar, oldInputMatch, oldInputCache) then
                newLabelChars = targets.labelChars
                for _, target in ipairs(targets) do
                    if test(target.char, inputMatch, matchCache) then
                        table.insert(newTargets, target)
                    end
                end
                break
            end
        end

        if #newTargets ~= 0 then
          curTargets = newTargets
          curLabelChars = newLabelChars
        end
    else
        for targetsChar, targets in pairs(wordStartTargetsByChar) do
            if test(targetsChar, inputMatch, matchCache) then
                curTargets = targets
                curLabelChars = targets.labelChars
                break
            end
        end
    end

    -- input character is not a 'word' character, but '=' or '[', etc.
    -- they are not displayed before the first input char because
    -- e.g. parentheses create too much noise
    if curTargets == nil then
        curTargets = get_targets(
            bufId, topLine, botLine,
            function(chars, i)
                return test(chars[i], inputMatch, matchCache)
                    and test_split_identifiers(chars, i)
            end
        )
        if #curTargets > 1 then
            local labels = options:get_typed_labels(inputChar)
            curTargets = assignTargetLabels(curTargets, cursorPos, labels, caseSensitive)
            curLabelChars = labels
        end
    end

    -- find the terget to jump to, jump to that target
    local i = 1
    while true do
        if #curTargets == 0 then
            vim.api.nvim_echo({{ 'no target', 'ErrorMsg' }}, true, {})
            break
        end
        if #curTargets == 1 then
            local label = curTargets[1]
            vim.fn.setpos('.', { 0, label.pos[1], label.pos[2], 0, label.pos[2] })
            break
        end

        clear()

        for _, target in ipairs(curTargets) do
            vim.api.nvim_buf_set_extmark(0, ns, target.pos[1]-1, target.pos[2]-1, {
                virt_text = {
                    { target.char, hl.typed_char },
                    { labelString(displayLabels, target.typedLabel), hl.typed_label },
                    { labelString(displayLabels, target.label), hl.rest_label },
                },
                virt_text_pos = 'overlay',
                hl_mode = 'combine'
            })
        end

        vim.cmd.redraw()
        inputChar = get_input()
        if inputChar == nil then break end

        local newTargets = {}
        local found

        for _, target in ipairs(curTargets) do
          if target.label.literal then
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

        if found then newTargets = { found } end

        curTargets = newTargets
        i = i + 1
    end
end

local function jump(opts)
    local options = createOptions(opts)
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
