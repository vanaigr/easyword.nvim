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
local matches = {
    upper = [=[\v[[:upper:]]\C]=],
    lower = [=[\v[[:lower:]]\C]=],
    digit = [=[\v[[:digit:]]\C]=],
    word  = [=[\v[[:upper:][:lower:][:digit:]]\C]=],
}

local test_cache -- :/   twice as slow otherwise
local function test(char, match)
    if char == nil then return false end -- vim.fn.match returns false for nil char, but not if pattern contains `[:lower:]`
    local key = char..match
    local value = test_cache[key]
    if value == nil then value = vim.fn.match(char, match) == 0 end
    test_cache[key] = value
    return value
end

local function splitByChars(str)
    return vim.fn.split(str, '\\zs\\ze')
end

local function toBoolean(value)
    if value then return true else return false end
end

local function test_split_identifiers(chars, cur_i)
    local cur_char = chars[cur_i]

    local is_match = false

    if test(cur_char, matches.upper) then
        local prev_char = chars[cur_i - 1]
        if not test(prev_char, matches.upper) then is_match = true
        else
            local next_char = chars[cur_i + 1]
            is_match = test(next_char, matches.word) and not test(next_char, matches.upper)
        end
    elseif test(cur_char, matches.digit) then
        is_match = not test(chars[cur_i - 1], matches.digit)
    elseif test(cur_char, matches.lower) then
        is_match = not test(chars[cur_i - 1], matches.word) or test(chars[cur_i - 1], matches.digit)
    else
        local prev_char = chars[cur_i - 1]
        is_match = prev_char ~= cur_char -- matching only first character in ==, [[ and ]]
    end

    return is_match
end

local function get_targets(winid, test_func)
    local wininfo = vim.fn.getwininfo(winid)[1]
    local bufId = vim.api.nvim_win_get_buf(winid)
    local lnum = wininfo.topline
    local botline = wininfo.botline

    local targets = {}

    while lnum <= botline do
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
    labels = {
        's', 'j', 'k', 'd', 'l', 'f', 'c', 'n', 'i', 'e', 'w', 'r', 'o', "'",
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

local function updList(table, update)
    for i, v in ipairs(update) do table[i] = v end
    return table
end

--- genetate variable length labels that use at most 2 characters without aaba, only aaab
--- order is left to right, top to bottom
local function computeLabels(labels, max)
    local list = {}
    for _, label in ipairs(labels) do table.insert(list, { label }) end

    -- TODO: ensure no infinite loop
    local curI = 1
    while #list < max do
        local sl = list[curI]
        local sst = sl[1]
        local sen = sl[#sl]
        if sst == sen then
            local i = 1
            while i <= #labels do
                if labels[i] == sst then break end
                i = i + 1
            end
            if i < #labels then
                table.remove(list, curI)
                while i <= #labels do
                    local newLabel = { unpack(sl) }
                    table.insert(newLabel, labels[i])
                    table.insert(list, newLabel)
                    i = i + 1
                end
            else
                curI = curI + 1
            end
        else
            curI = curI + 1
        end
    end

    return list
end

local function labelString(label)
    return table.concat(label, '')
end

local function isPosBefore(pos, otherPos)
    return pos[1] < otherPos[1] or (pos[1] == otherPos[1] and pos[2] < otherPos[2])
end

--- index of first target >= position
local function findPosition(targets, position)
    local begin = 1
    local en = #targets + 1
    while begin < en do
        local m = begin + math.floor((en - begin) / 2)
        local v = targets[m]
        if isPosBefore(v.pos, position) then
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

local function findCharKey(map, targetChar, makeCharMatch)
    if map[targetChar] then
        return targetChar, map[targetChar]
    else
        local match = makeCharMatch(targetChar)
        for char, data in pairs(map) do
            if test(char, match) then
                return char, data
            end
        end
    end
end

local function assignTargetLabels(targets, cursorPos, options)
    targets = sortTargets(cursorPos, targets)
    local filteredLabels = {}

    local first = targets[1]
    local targetStart = 1
    if first then
        first.typedLabel = {}
        targetStart = 2

        -- should be case insensitive comparison, but I am tired
        -- of using vim regular expressions for everything
        -- character would need to be escaped
        local fChar = first.char:lower()

        local fAfter = isPosBefore(cursorPos, first.pos)
        if not fAfter or fChar == options.labels[1]:lower() then
            if fAfter then
                first.caseInsensitive = true
                first.label = { fChar }
            else
                first.label = { options.labels[1] }
            end

            for i = 2, #options.labels do
                table.insert(filteredLabels, options.labels[i])
            end
        else
            first.label = { fChar }
            first.caseInsensitive = true

            local second = targets[2]
            local labelStart = 1
            if second and isPosBefore(second.pos, cursorPos) then
                second.label = { options.labels[1] }
                second.typedLabel = {}
                labelStart = 2
                targetStart = 3
            end

            for i = labelStart, #options.labels do
                if options.labels[i]:lower() ~= fChar then
                    table.insert(filteredLabels, options.labels[i])
                end
            end
        end
    end

    local labels = computeLabels(filteredLabels, #targets - targetStart + 1)
    for i = 1, #targets - targetStart + 1 do
        local target = targets[targetStart + i - 1]
        target.label = labels[i]
        target.typedLabel = {}
    end

    return targets
end

local Timer = {}
Timer.__index = Timer
function Timer:add(name)
    table.insert(self, { os.clock(), name })
end
function Timer:new()
    local o = {}
    setmetatable(o, Timer)
    return o
end
function Timer:print()
    local prev
    for _, data in ipairs(self) do
        if prev then
            print(data[2], vim.fn.round((data[1] - prev[1]) * 100000)/100)
        end
        prev = data
    end

end

local function jumpToWord(options)
    local hl = options.highlight
    local ns = options.namespace

    local winid = vim.api.nvim_get_current_win()
    local bufId = vim.api.nvim_win_get_buf(winid)
    local lastLine = vim.api.nvim_buf_line_count(bufId) - 1

    local cursorPos = vim.fn.getpos('.')
    cursorPos = { cursorPos[2], cursorPos[3] }

    local wordStartTargets = get_targets(
        winid, function(chars, i)
            return test(chars[i], matches.word) and test_split_identifiers(chars, i)
        end
    )

    -- remove labels at cursor position
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

    local caseSensitive = toBoolean(options.case_sensitive)
    local function makeCharMatch(char)
        if caseSensitive then
            return '\\v[[='..char..'=]]\\C'
        else
            return '\\v[[='..char..'=]]\\c'
        end
    end

    local wordStartTargetsByChar = {}
    for _, target in ipairs(wordStartTargets) do
        local _, curData = findCharKey(wordStartTargetsByChar, target.char, makeCharMatch)
        if not curData then
            curData = {}
            wordStartTargetsByChar[target.char] = curData
        end
        table.insert(curData, target)
    end

    vim.api.nvim_buf_clear_namespace(0, ns, 0, -1)
    vim.highlight.range(bufId, ns, hl.backdrop, { 0, 0 }, { lastLine, -1 }, { })

    for key, targets in pairs(wordStartTargetsByChar) do
        if #targets == 1 then
            local target = targets[1]
            vim.api.nvim_buf_set_extmark(0, ns, target.pos[1]-1, target.pos[2]-1, {
                virt_text = { { target.char, hl.unique } },
                virt_text_pos = 'overlay',
                hl_mode = 'combine'
            })
        else
            targets = assignTargetLabels(targets, cursorPos, options)
            wordStartTargetsByChar[key] = targets
            for _, target in ipairs(targets) do
                vim.api.nvim_buf_set_extmark(0, ns, target.pos[1]-1, target.pos[2]-1, {
                    virt_text = {
                        { target.char, hl.rest_char },
                        { labelString(target.label), hl.rest_label },
                    },
                    virt_text_pos = 'overlay',
                    hl_mode = 'combine'
                })
            end
        end
    end

    vim.cmd.redraw()
    local char = get_input()
    if char == nil then return end

    local sensitivityChanged = false
    if not caseSensitive and options.smart_case then
        local upperChar = splitByChars(vim.fn.toupper(char))
        local lowerChar = splitByChars(vim.fn.tolower(char))
        -- if both cases can be typed, are different, and current character is uppper case
        local newCaseSensitive = #upperChar == 1 and #lowerChar == 1
            and upperChar[1] ~= lowerChar[1] and char == upperChar[1]
        sensitivityChanged = newCaseSensitive ~= caseSensitive
        caseSensitive = newCaseSensitive
    end

    local inputMatch = makeCharMatch(char)

    local curTargets
    if sensitivityChanged then
        local newTargets = {}
        for _, targets in pairs(wordStartTargetsByChar) do
            for _, target in ipairs(targets) do
                if test(target.char, inputMatch) then
                    table.insert(newTargets, target)
                end
            end
        end
        if #newTargets ~= 0 then curTargets = newTargets end
    else
        for targetsChar, targets in pairs(wordStartTargetsByChar) do
            if test(targetsChar, inputMatch) then
                curTargets = targets
                break
            end
        end
    end
    if curTargets == nil then
        curTargets = get_targets(
            winid,
            function(chars, i)
                local t1 = test(chars[i], inputMatch)
                local t2 = test_split_identifiers(chars, i)
                return t1 and t2
            end
        )
        if #curTargets > 1 then
            curTargets = assignTargetLabels(curTargets, cursorPos, options)
        end
    end

    local i = 1
    while true do
        if #curTargets == 0 then
            vim.api.nvim_echo({{ 'no label', 'ErrorMsg' }}, true, {})
            break
        end
        if #curTargets == 1 then
            local label = curTargets[1]
            vim.fn.setpos('.', { 0, label.pos[1], label.pos[2], 0, label.pos[2] })
            break
        end

        vim.api.nvim_buf_clear_namespace(0, ns, 0, -1)
        vim.highlight.range(bufId, ns, hl.backdrop, { 0, 0 }, { lastLine, -1 }, { })

        for _, target in pairs(curTargets) do
            vim.api.nvim_buf_set_extmark(0, ns, target.pos[1]-1, target.pos[2]-1, {
                virt_text = {
                    { target.char, hl.typed_char },
                    { labelString(target.typedLabel), hl.typed_label },
                    { labelString(target.label), hl.rest_label },
                },
                virt_text_pos = 'overlay',
                hl_mode = 'combine'
            })
        end

        vim.cmd.redraw()
        char = get_input()
        if char == nil then break end

        local newTargets = {}
        local found

        for _, target in ipairs(curTargets) do
            for targetI, labelChar in ipairs(target.label) do
                if char == labelChar or (
                    -- case insens -> labelChar is lower
                    target.caseInsensitive and char:lower() == labelChar
                ) then
                    if #target.label == 1 then
                        found = target
                    else
                        table.insert(newTargets, target)
                        table.remove(target.label, targetI)
                        table.insert(target.typedLabel, labelChar)
                    end
                    break
                end
            end
            if found then break end
        end

        if found then newTargets = { found } end

        curTargets = newTargets
        i = i + 1
    end
end

local function jump(opts)
    local options = createOptions(opts)
    test_cache = {}
    local ok, result = pcall(jumpToWord, options)
    if not ok then vim.api.nvim_echo({{'Error: '..vim.inspect(result), 'ErrorMsg'}}, true, {}) end
    vim.api.nvim_buf_clear_namespace(0, options.namespace, 0, -1)
end

applyDefaultHighlight()

return {
    options = defaultOptions,
    apply_default_highlight = applyDefaultHighlight,
    jump = jump,
}
