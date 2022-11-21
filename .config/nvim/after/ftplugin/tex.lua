require('cmp').setup.buffer {
  formatting = {
    format = function(entry, vim_item)
      vim_item.menu = ({
        omni = (vim.inspect(vim_item.menu):gsub('%"', "")),
        buffer = "[Buffer]",
        -- formatting for other sources
      })[entry.source.name]
      return vim_item
    end,
  },
  sources = {
    { name = 'omni' },
    { name = 'vsnip' },
    { name = 'buffer' },
    { name = 'path' },
    -- other sources
  },
}

vim.g.vimtex_format_enabled = 1
vim.g.vimtex_quickfix_mode = 0
vim.g.vimtex_quickfix_open_on_warning = 0
vim.g.vimtex_quickfix_ignore_filters = { 'Overfull', 'Underfull', }
vim.g.vimtex_view_general_viewer = 'evince'
vim.g.vimtex_compiler_latexmk = {
  build_dir = '',
  callback = 1,
  continuous = 1,
  executable = 'latexmk',
  hooks = {},
  options = {
    '-verbose',
    '-file-line-error',
    '-synctex=1',
    '-interaction=nonstopmode',
  },
}

-- vim.keymap.set('n', '<localleader>t', '<cmd>:call vimtex#fzf#run()<CR>', { desc = 'TOC', buffer = true })
-- vim.keymap.set('n', '<localleader>f', '<cmd> vimtex#fzf#run()<CR>', { desc = 'TOC', buffer = true })
vim.keymap.set('n', '<localleader>v', '<cmd>:call SVED_Sync()<CR>', { desc = 'View', buffer = true })
