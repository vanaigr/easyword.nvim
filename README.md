# WIP
Prototype that demonstates how [leap-by-word](https://github.com/Sleepful/leap-by-word.nvim)
could've worked if it was possible to add searching logic to custom targets in 
[Leap.nvim](https://github.com/ggandor/leap.nvim)

# Usage

```lua
vim.keymap.set({ 'n', 'x', 'o' }, 's', function()
    require('nvim-easyword').jump()
end)
```
