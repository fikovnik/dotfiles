vim.defer_fn(function()
  pcall(require, "impatient")
end, 0)

require('options')
require('autocmds')

-- add binaries installed by mason.nvim to path
local is_windows = vim.loop.os_uname().sysname == "Windows_NT"
vim.env.PATH = vim.env.PATH .. (is_windows and ";" or ":") .. vim.fn.stdpath "data" .. "/mason/bin"

local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  print 'Cloning packer ..'
  fn.system { 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path }
  require('plugins')
  vim.cmd [[PackerSync]]
else
  require('plugins')
end

require('keybinds')

-- https://github.com/nanotee/nvim-lua-guide
