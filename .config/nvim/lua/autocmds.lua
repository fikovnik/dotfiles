local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

augroup('MyAutoGroup', { clear = true })

-- highlight on yank
autocmd('TextYankPost', {
  group = 'MyAutoGroup',
  callback = function()
    vim.highlight.on_yank({ higroup = 'IncSearch', timeout = '1000' })
  end
})

-- automatically run :PackerCompile whenever plugins.lua is updated
autocmd('BufWritePost', {
  command = 'source <afile> | PackerCompile',
  group = vim.api.nvim_create_augroup('PACKER', { clear = true }),
  pattern = 'plugins.lua',
})

-- dont list quickfix buffers
autocmd("FileType", {
  group = 'MyAutoGroup',
  callback = function() vim.opt_local.buflisted = false end,
  pattern = "qf",
})

-- create missing directories
autocmd('BufWritePre', {
  group = 'MyAutoGroup',
  pattern = '*',
  callback = require('utils').mkdir_for_current_file,
})

-- set spell in git
autocmd("FileType", {
  group = 'MyAutoGroup',
  callback = function()
    vim.opt_local.spell = true
  end,
  pattern = { "gitcommit" }
})

-- use TAB for = in fugitive
autocmd('FileType', {
  group = 'MyAutoGroup',
  callback = function() vim.keymap.set('n', '<TAB>', '=', { buffer = true, silent = true, remap = true }) end,
  pattern = 'fugitive',
})

-- quit using `q`
autocmd('FileType', {
  group = 'MyAutoGroup',
  callback = function() vim.keymap.set('n', 'q', '<cmd>q<CR>', { buffer = true }) end,
  pattern = { 'fugitive', 'git', 'qf', 'help' }
})

-- set compiler to cargo for rust
autocmd('FileType', {
  group = 'MyAutoGroup',
  callback = function() vim.opt_local.makeprg = 'cargo' end,
  pattern = 'rust'
})
