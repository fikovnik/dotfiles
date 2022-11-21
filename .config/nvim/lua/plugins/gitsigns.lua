local map = vim.keymap.set

require('gitsigns').setup {
  on_attach = function(buf)
    local gs = package.loaded.gitsigns
    local opts = { buffer = buf, expr = true, replace_keycodes = false }

    -- Navigation
    map('n', ']c', "&diff ? ']c' : '<CMD>Gitsigns next_hunk<CR>'", opts)
    map('n', '[c', "&diff ? '[c' : '<CMD>Gitsigns prev_hunk<CR>'", opts)

    -- Actions
    map({ 'n', 'v' }, '<leader>ghr', gs.reset_hunk, { buffer = buf })
    map('n', '<leader>ghp', gs.preview_hunk, { buffer = buf })
    map('n', '<leader>gb', gs.blame_line, { buffer = buf })

    -- Text object
    map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>', { buffer = buf })
  end,
}
