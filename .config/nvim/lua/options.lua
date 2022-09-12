local opt = vim.opt
local g = vim.g

-- global statusline
opt.laststatus = 3
opt.showmode = false
opt.title = true

-- indenting
opt.expandtab = true
opt.shiftwidth = 2
opt.smartindent = true
opt.tabstop = 2
opt.softtabstop = -1 -- use shiftwidth

opt.fillchars = { eob = " ", diff = "/" }
opt.ignorecase = true
opt.smartcase = true

-- because of nvim-cmp
opt.completeopt = "menuone,noselect"

-- numbers
opt.number = true
opt.numberwidth = 3
opt.ruler = false

-- disable nvim intro
opt.shortmess:append('sI')
opt.shortmess:remove('F')

opt.signcolumn = "yes"
opt.splitbelow = true
opt.splitright = true
opt.termguicolors = true
opt.timeoutlen = 400
opt.undofile = true

-- interval for writing swap file to disk, also used by gitsigns
opt.updatetime = 250

-- go to previous/next line with h,l,left arrow and right arrow
-- when cursor reaches end/beginning of line
opt.whichwrap:append("<>[]hl")

opt.hlsearch = false
opt.mouse = ''
opt.background = 'dark'
opt.scrolloff = 5
opt.signcolumn = 'yes:2'
opt.cursorline = true
opt.listchars = { eol = '↲', tab = '▸ ', trail = '·' }
-- vim.o.list = true
-- vim.o.jumpoptions = 'view'

opt.path:append('**')
opt.wildmenu = true

g.mapleader = ' '
g.maplocalleader = 'm'

-- use filetype.lua instead of filetype.vim. it's enabled by default in neovim 0.8 (nightly)
if vim.version().minor < 8 then
  g.did_load_filetypes = 0
  g.do_filetype_lua = 1
end

-- disable some builtin vim plugins
local default_plugins = {
  "2html_plugin",
  "getscript",
  "getscriptPlugin",
  "gzip",
  "logipat",
  "netrw",
  "netrwPlugin",
  "netrwSettings",
  "netrwFileHandlers",
  "matchit",
  "tar",
  "tarPlugin",
  "rrhelper",
  "spellfile_plugin",
  "vimball",
  "vimballPlugin",
  "zip",
  "zipPlugin",
  "tutor",
  "rplugin",
  "syntax",
  "synmenu",
  "optwin",
  "compiler",
  "bugreport",
  "ftplugin",
}

for _, plugin in pairs(default_plugins) do
  g["loaded_" .. plugin] = 1
end

local default_providers = {
  "node",
  "perl",
  "python3",
  "ruby",
}

for _, provider in ipairs(default_providers) do
  vim.g["loaded_" .. provider .. "_provider"] = 0
end

if os.getenv('SSH_CONNECTION') then
  vim.g.clipboard = {
    name = 'osc52',
    copy = { ['+'] = require('utils').osc52_copy, ['*'] = require('utils').osc52_copy },
    paste = { ['+'] = require('utils').osc52_paste, ['*'] = require('utils').osc52_paste },
  }
end
