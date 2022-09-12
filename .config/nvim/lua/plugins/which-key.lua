local present, wk = pcall(require, 'which-key')

if not present then
  return
end

wk.setup {
  key_labels = {
    ['<space>'] = 'SPC',
    ['<CR>'] = 'RET',
    ['<tab>'] = 'TAB',
  },
}

wk.register {
  ['<leader>'] = {
    b = { name = 'buffers' },
    e = { name = 'edit' },
    f = { name = 'files' },
    g = { name = 'git' },
    o = { name = 'open' },
    t = { name = 'toggle' },
    w = { name = 'window' },
  },
}
