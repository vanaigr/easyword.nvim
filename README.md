# Usage

```lua
vim.keymap.set('n', 's', function()
    require('easyword').jump({ recover_key = 's' })
end)
vim.keymap.set({ 'x', 'o' }, 'x', function()
    require('easyword').jump({ recover_key = 'x' })
end)
```
