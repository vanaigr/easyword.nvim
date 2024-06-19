Jump to a word by typing its first letter and a label.
Most labels are shown right away and filtered out as you type.

https://github.com/vanaigr/easyword.nvim/assets/65824523/5eaeace7-7b27-4d8c-a08c-2a95730892da

# Installation

Use your preferred plugin manager. No extra steps required.

# Usage

```lua
local easyword = require('easyword')
easyword.apply_default_highlight()

vim.keymap.set('n', 's', function() easyword.jump{ recover_key = 's' } end)
vim.keymap.set('x', 'x', function() easyword.jump{ recover_key = 'x' } end)
vim.keymap.set('o', 'x', function() easyword.jump() end)

local group = vim.api.nvim_create_augroup('EasywordHighlighting', { clear = true })
vim.api.nvim_create_autocmd('ColorScheme', {
    callback = function() easyword.apply_default_highlight() end,
    pattern = '*', group = group,
})
```

# Notes

* The label for the first target after the cursor is the target character itself.
That is, to jump to the first target, type the target character twice. If there
are no targets after the cursor, the first target before the cursor is used.
* Labels are specified with the `labels` option:
`easyword.jump{ labels = { 'a', 'b', 'c' } }`.
There must be a minimum of 3 label characters, and all characters must be distinct.
* The `target_display` option determines the substitute target characters for display.
By default, `\n` is shown as a space. All label characters must occupy one display cell.
Example: `function(char) if char == '\n' then return ' ' else return char end end`.

## Case and accent sensitivity

All labels, jump target characters and input characters are normalized,
enabling different characters to be grouped together when their normalized forms match.
Normalization function is provided through `char_normalize` field of the options table.
```lua
easyword.jump{ char_normalize = your_function }
```

Default normalization function is case and accent insensitive,
with tab, newline and carriage return being equivalent to space.

<details>

<summary>Default character normalization function (case insensitive, accent insensitive)</summary>

```lua
local normFunction
do
    local normList = {}
    local charRegex = {}

    normList['\t'] = ' '
    normList['\n'] = ' '
    normList['\r'] = ' '
    for i = 32, 64 do
        local ch = string.char(i)
        charRegex[ch] = vim.regex('^[[='..ch..'=]]$\\c')
        normList[ch] = ch
    end
    for i = 1, 26 do -- A-Z => a-z
        normList[string.char(64 + i)] = string.char(96 + i)
    end
    for i = 91, 126 do
        local ch = string.char(i)
        charRegex[ch] = vim.regex('^[[='..ch..'=]]$\\c')
        normList[ch] = ch
    end

    normFunction = function(char)
        local v = normList[char]
        if v then return v end

        for k, pattern in pairs(charRegex) do
            if pattern:match_str(char) then
                normList[char] = k
                return k
            end
        end

        -- Add a new character to the list
        charRegex[char] = vim.regex('^[[='..char..'=]]$\\c')
        normList[char] = char
        return char
    end
end
```

</details>

<details>

<summary>Case sensitive, accent insensitive normalization</summary>

```lua
local normFunction
do
    local normList = {}
    local charRegex = {}

    normList['\t'] = ' '
    normList['\n'] = ' '
    normList['\r'] = ' '
    for i = 32, 126 do
        local ch = string.char(i)
        charRegex[ch] = vim.regex('^[[='..ch..'=]]$\\C')
        normList[ch] = ch
    end

    normFunction = function(char)
        local v = normList[char]
        if v then return v end

        for k, pattern in pairs(charRegex) do
            if pattern:match_str(char) then
                normList[char] = k
                return k
            end
        end

        charRegex[char] = vim.regex('^[[='..char..'=]]$\\C')
        normList[char] = char
        return char
    end
end
```

</details>

<details>

<summary>Case insensitive, accent sensitive normalization</summary>

```lua
local normFunction
do
    local normList = {}
    local charRegex = {}

    normList['\t'] = ' '
    normList['\n'] = ' '
    normList['\r'] = ' '
    for i = 32, 64 do
        local ch = string.char(i)
        normList[ch] = ch
    end
    for i = 1, 26 do -- A-Z => a-z
        normList[string.char(64 + i)] = string.char(96 + i)
    end
    for i = 91, 96 do
        local ch = string.char(i)
        normList[ch] = ch
    end
    for i = 97, 122 do
        local ch = string.char(i)
        charRegex[ch] = vim.regex('^[[.'..ch..'.]]$\\c')
    end
    for i = 123, 126 do
        local ch = string.char(i)
        normList[ch] = ch
    end

    normFunction = function(char)
        local v = normList[char]
        if v then return v end

        for k, pattern in pairs(charRegex) do
            if pattern:match_str(char) then
                normList[char] = k
                return k
            end
        end

        charRegex[char] = vim.regex('^[[.'..char..'.]]$\\c')
        normList[char] = char
        return char
    end
end
```

</details>

<details>

<summary>Case sensitive, accent sensitive normalization</summary>

```lua
local normFunction
do
    local normList = {}
    normList['\t'] = ' '
    normList['\n'] = ' '
    normList['\r'] = ' '

    normFunction = function(char)
        local v = normList[char]
        if v then return v end

        return char
    end
end
```

</details>

These functions map all the characters, even the ones that can't be typed on a regular 'qwerty' keyboard.
You can return some fallback character in case the `char` is not in the original list,
or an empty string so that the character would not considered a valid target.
Don't forget to add it to the cache first!

### Multiple keyboard layouts

You can also map characters from different keyboard layouts to your primary layout.
This allows jumping to characters from a different keyboard layout without actually switching to it.

<details>

<summary>Case insensitive, accent insensitive normalization, 'йцукен' mapped to 'qwerty'</summary>

```lua
local normFunction
do
    local normList = {}
    local charRegex = {}

    normList['\t'] = ' '
    normList['\n'] = ' '
    normList['\r'] = ' '
    for i = 32, 64 do
        local ch = string.char(i)
        charRegex[ch] = vim.regex('^[[='..ch..'=]]$\\c')
        normList[ch] = ch
    end
    for i = 1, 26 do -- A-Z => a-z
        normList[string.char(64 + i)] = string.char(96 + i)
    end
    for i = 91, 126 do
        local ch = string.char(i)
        charRegex[ch] = vim.regex('^[[='..ch..'=]]$\\c')
        normList[ch] = ch
    end

    local qwerty    = vim.fn.split([==[qwertyuiop[]asdfghjkl;'zxcvbnm,.`]==], '\\zs')
    local cyrillic  = vim.fn.split([==[йцукенгшщзхъфывапролджэячсмитьбюё]==], '\\zs')
    local cyrillicU = vim.fn.split([==[ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮЁ]==], '\\zs')

    -- no regex since vim doesn't support equivalence classes for cyrillic
    for i = 1, #cyrillic do
        normList[cyrillic [i]] = qwerty[i]
        normList[cyrillicU[i]] = qwerty[i]
    end
    normList['№'] = '#'

    normFunction = function(char)
        local v = normList[char]
        if v then return v end

        for k, pattern in pairs(charRegex) do
            if pattern:match_str(char) then
                normList[char] = k
                return k
            end
        end

        -- Map all other characters to space
        normList[char] = ' '
        return ' '
    end
end
```

</details>

## Recover key

Once the jump is finished, whether normally or with an error,
the plugin allows you to redo the same jump with the same targets and labels.
This functionality is enabled with the `recover_key` option,
which specifies the sequence of keys needed to redo the jump.

For simple keys (letters, numbers, etc.), a regular string can be used.
For keys with CTRL, use `nvim_replace_termcodes`:
```lua
vim.api.nvim_replace_termcodes('<C-key>more keys', true, false, true)
```

## Highlight groups and namespaces

You can specify your own highlight group names with `highlight` option.
Default highlight groups:
```lua
backdrop = 'EasywordBackdrop'
target_first = 'EasywordTargetFirst'
target_first_typed = 'EasywordTargetFirstTyped'
rest_char = 'EasywordRestChar'
typed_char = 'EasywordTypedChar'
rest_label = 'EasywordRestLabel'
typed_label = 'EasywordTypedLabel'
```

Namespace can be specified with the `namespace` option.
Default namespace is 'Easyword'.

# Acknowledgments

[hop.nvim](https://github.com/smoka7/hop.nvim)

[leap-by-word.nvim](https://github.com/Sleepful/leap-by-word.nvim)

[leap.nvim](https://github.com/ggandor/leap.nvim)
