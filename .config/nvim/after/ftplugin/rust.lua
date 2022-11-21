vim.opt_local.makeprg = 'cargo'
vim.keymap.set('n', '<localleader>c', '<cmd>RustOpenCargo<CR>', { desc = 'Open Cargo' })
vim.keymap.set('n', '<localleader>wr', '<cmd>RustReloadWorkspace<CR>', { desc = 'Reload' })
vim.keymap.set('n', '<localleader>?', '<cmd>RustOpenExternalDocs<CR>', { desc = 'External docs' })
vim.keymap.set('n', '<localleader>?', '<cmd>RustOpenExternalDocs<CR>', { desc = 'External docs' })
vim.keymap.set('n', 'K', '<cmd>RustHoverActions<CR>', { desc = 'Hover actions' })
