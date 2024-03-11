Jump to any word on the screen by typing its first letter and an optional label.
Most targets are shown right away, and then filtered out as you type.

https://github.com/vanaigr/easyword.nvim/assets/65824523/5eaeace7-7b27-4d8c-a08c-2a95730892da

# Installation

Use your preferred plugin manager. No extra steps required.

# Usage

```lua
local easyword = require('easyword')
easyword.apply_default_highlight()

vim.keymap.set('n'         , 's', function() easyword.jump{ recover_key = 's' } end)
vim.keymap.set({ 'x', 'o' }, 'x', function() easyword.jump{ recover_key = 'x' } end)

local group = vim.api.nvim_create_augroup('EasywordHighlighting', { clear = true })
vim.api.nvim_create_autocmd('ColorScheme', {
    callback = function() easyword.apply_default_highlight() end,
    pattern = '*', group = group,
})
```

# Acknowledgments

[hop.nvim](https://github.com/smoka7/hop.nvim)

[leap-by-word.nvim](https://github.com/Sleepful/leap-by-word.nvim)

[leap.nvim](https://github.com/ggandor/leap.nvim)
