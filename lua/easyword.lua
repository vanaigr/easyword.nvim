local vim = vim
local unpack = table.unpack or unpack

local bit = require('bit')

local debug = false
local map = false

local pcall = pcall
if debug then pcall = function(f, ...) return true, f(...) end end

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

local patterns = { vim.regex('^[[:digit:]]$'), vim.regex('^[[:lower:]]$'), vim.regex('^[[:upper:]]$'), }
local categoriesCache = {}

-- populate cache
for i = 0, 127 do
    local char = string.char(i)
    for c, p in ipairs(patterns) do
        if p:match_str(char) ~= nil then
            categoriesCache[char] = c
            goto next
        end
    end
    categoriesCache[char] = 0
    ::next::
end

local function category(char)
    if char == nil then return 0 end

    local value = categoriesCache[char]
    if value ~= nil then return value end

    for c, p in ipairs(patterns) do
        if p:match_str(char) ~= nil then
            categoriesCache[char] = c
            return c
        end
    end

    categoriesCache[char] = 0
    return 0
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
            if not nextByte then return result end
            byte = nextByte
            i = i + 1
        end
    end
    vim.list_extend(result, vim.fn.split(str:sub(i), '\\zs'))
    return result
end

--- functions for finding targets ---

-- v   v   v     v    v    v
-- wordWORDWord  word_WORD WORD ==
--                   ^          ^
local function test_split_identifiers(chars, cur_i)
    local cur_char = chars[cur_i]
    local prev_char = chars[cur_i - 1]

    local curC = category(cur_char)
    if curC == 2 then -- lower
        return category(prev_char) < 2
    elseif curC == 3 then --upper
        if category(prev_char) ~= 3 then return true end
        local nextC = category(chars[cur_i + 1])
        return nextC == 1 or nextC == 2
    elseif curC == 1 then -- digit
        return category(prev_char) ~= 1
    else
        return cur_char ~= prev_char -- matching only first character in ==, [[ and ]]
    end
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
            table.insert(chars, '\n')

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
            assert(string.len(line) + 1 == col - 1)

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

    for i = 1, 8 do
        local ch = string.char(i)
        normCache[ch] = ch
    end
    normCache['\t'] = ' '
    normCache['\n'] = ' '
    for i = 11, 31 do
        local ch = string.char(i)
        normCache[ch] = ch
    end
    for i = 32, 64 do
        local ch = string.char(i)
        chars[ch] =  vim.regex('^[[='..ch..'=]]\\c$')
        normCache[ch] = ch
    end
    for i = 1, 26 do -- A-Z => a-z
        normCache[string.char(64 + i)] = string.char(96 + i)
    end
    for i = 91, 126 do
        local ch = string.char(i)
        chars[ch] = vim.regex('^[[='..ch..'=]]\\c$')
        normCache[ch] = ch
    end
    do
        local ch = string.char(127)
        normCache[ch] = ch
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

local defaultOptions = {
    -- must be all unique and 1 cell wide. #labels >= 3
    labels = defaultLabels,
    normalizedLabels = defaultLabels,
    char_normalize = defaultCharNormalize,
    target_display = { ['\n'] = ' ' },
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
    result.target_display = opts.target_display or defaultOptions.target_display

    local l = opts.labels
    if l then result.labels = vim.list_extend({}, l)
    else result.labels = defaultOptions.labels end

    -- TODO: check if normalized targets are different
    if result.char_normalize == defaultOptions.char_normalize then
        result.normalizedLabels = defaultOptions.normalizedLabels
    else
        result.normalizedLabels = {}
        for k, v in ipairs(result.labels) do
            result.normalizedLabels[k] = result.char_normalize(v)
        end
    end

    local t = opts.special_targets
    if t then result.special_targets = { unique = toBoolean(t.unique), first = toBoolean(t.first) }
    else result.special_targets = defaultOptions.special_targets end

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
-- labels would contain max * { { repCount, repCharI, lastCharI }, ... }
local function computeLabels(sameCharLabels, sameC, max, labels)
    if #sameCharLabels < 2 then
        if max == 0 then return {} end
        if #sameCharLabels == max then return sameCharLabels end
        error('could not generate ' .. max .. ' labels from ' .. #sameCharLabels .. ' label variations')
    end

    local regularEnd, sameI = 0, 0
    while regularEnd + sameC < max do
        local curLabel = sameCharLabels[sameI + 1]
        local labelChar = curLabel[2]
        local curLabelLen = curLabel[1] + 1

        for i = 1, sameC do
            local oChar = sameCharLabels[i][2]
            if oChar ~= labelChar then
                labels[regularEnd + 1] = { curLabelLen, labelChar, oChar }
                regularEnd = regularEnd + 1
            end
        end

        curLabel[1] = curLabel[1] + 1
        sameI = sameI + 1
        if sameI >= #sameCharLabels then sameI = 0 end
    end

    -- Merge same char array and regular labels. Same char labels array is split by sameI into 2 parts
    -- where all labels have same length, and lower part labels have length one more than upper part.

    -- Avoiding possible penalty for holes in array (extra -1 because last is overwritten first anyway)
    for i = #labels + 1, regularEnd + sameC - 2 do labels[i] = 0 end

    local sameLC, sameUC = sameI, sameC - sameI -- lower count, upper count

    if sameLC ~= 0 then -- lower part
        local sameV = sameCharLabels[1][1]
        while regularEnd > 0 do
            local l = labels[regularEnd] -- '- 1 + 1'
            if l[1] < sameV then break end
            labels[regularEnd + sameC] = l -- '- 1 + 1'
            regularEnd = regularEnd - 1
        end
        for i = 1, sameLC do labels[regularEnd + sameUC + i] = sameCharLabels[i] end
    end

    -- upper part
    local sameV = sameCharLabels[sameLC + 1][1]
    while regularEnd > 0 do
        local l = labels[regularEnd] -- '- 1 + 1'
        if l[1] < sameV then break end
        labels[regularEnd + sameUC] = l -- '- 1 + 1'
        regularEnd = regularEnd - 1
    end
    for i = 1, sameUC do labels[regularEnd + i] = sameCharLabels[sameLC + i] end
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

    local char = options.target_display[target.char] or target.char

    local virt_text
    if not l then -- if first target (may be special) (unique rendered separately)
        if is_special.unique and target.unique then
            virt_text = { { char, hl.unique } }
        elseif is_special.first then
            virt_text = { { char, choose(stage == 0, hl.target_first, hl.target_first_typed) }}
        else
            virt_text = {
                { char, choose(stage == 0, hl.rest_char, hl.typed_char) },
                { char, hl.rest_label },
            }
        end
    elseif stage == 0 then
        virt_text = {
            { char, hl.rest_char },
            { displayLabels[l[2]]:rep(l[1]) .. displayLabels[l[3]], hl.rest_label },
        }
    elseif stage - 1 > l[1] then
        virt_text = {
            { char, hl.typed_char },
            { displayLabels[l[2]]:rep(l[1]) .. displayLabels[l[3]], hl.typed_label },
        }
    else
        virt_text = {
            { char, hl.typed_char },
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
        if (is_special.unique and target.unique) or is_special.first then
            return 0
        else
            return 1
        end
    else
        return target.label[1] + 1
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
    local timer
    if debug then timer = Timer:new(); timer:add('') end

    local is_special = options.special_targets

    local winid = vim.api.nvim_get_current_win()
    local bufId = vim.api.nvim_get_current_buf()

    local wininfo = vim.fn.getwininfo(winid)[1]
    local topLine = wininfo.topline
    local botLine = wininfo.botline

    local cursorPos = vim.fn.getpos('.')
    local cursorLine, cursorCol = cursorPos[2], cursorPos[3]

    if debug then timer:add('prep') end

    -- find all tragets
    local wordStartTargets = get_targets(bufId, topLine, botLine)
    if debug then timer:add('targets') end

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

    if debug then timer:add('remove at cursor') end

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

    if debug then timer:add('by word') end

    local sameCharLabels, labels = {}, {}
    -- assign labels to groups of targets
    for charN, targets in pairs(wordStartTargetsByChar) do
        assert(#targets > 0)

        local priority
        if charN == ' ' or charN == '\t' then priority = 2 -- Note: there are more spaces...
        elseif category(charN) <= 0 then priority = 1
        else priority = 0 end

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

            local sameC = 0
            for i, v in ipairs(options.normalizedLabels) do
                if charN ~= v then
                    sameCharLabels[sameC + 1] = { 0, i, i }
                    sameC = sameC + 1
                end
            end
            computeLabels(sameCharLabels, sameC, #targets - 1, labels)
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

    if debug then timer:add('labels') end

    -- remove targets with intersecting labels
    -- TODO: reassign labels? (should all be shorter than before)
    if #wordStartTargets > 1 then
        -- Go through the visible targets once for each priority
        -- (starting from highest) and remove targets at that priority
        -- if they intersect with other targets with priority <= cur.

        local prev
        -- Compute end bound (inclusive). Assumes all chars are length 1
        for i, t in ipairs(wordStartTargets) do
            local start = t.charI
            t.charEndI = t.charI + targetLabelLen(t, options) -- + 1(target char width) - 1

            if t.priority == 2 then t.hidden = true -- skipping all whitespace
            elseif not t.hidden and t.priority == 0 then
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
        do -- Note: stage 2 is skipped, since all targets are hidden
            local stage = 1

            -- find starting target (visible, more important or same priority as current)
            local prevI = 1
            local prev = wordStartTargets[prevI]
            while prev and (prev.hidden or prev.priority < stage) do
                prevI = prevI + 1
                prev = wordStartTargets[prevI]
            end

            local i = prevI + 1
            while i <= #wordStartTargets do
                local cur = wordStartTargets[i]
                if not cur.hidden and cur.priority <= stage then
                    if cur.line == prev.line and cur.charI <= prev.charEndI then
                        -- it's better to keep prev if priorities are equal and it stops at same column?
                        if prev.priority > cur.priority or prev.charEndI > cur.charEndI then
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

    if debug then timer:add('remove overlap'); timer:print(); print(#wordStartTargets) end

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

    local t
    if debug then t = Timer:new(); t:add('') end

    for _, target in ipairs(wordStartTargets) do
        if not target.hidden then
            displayLabel(target, options, 0)
        end
    end

    if debug then t:add('targets'); t:print() end

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

        for i = 1, lastCurTarget do
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

if map then vim.keymap.set('n', 's', jump) end

return {
    options = defaultOptions,
    apply_default_highlight = applyDefaultHighlight,
    jump = jump,
}
