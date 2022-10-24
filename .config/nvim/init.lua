vim.defer_fn(function()
  pcall(require, "impatient")
end, 0)

require('options')
require('autocmds')
require('plugins')
require('keybinds')
