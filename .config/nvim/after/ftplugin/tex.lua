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

vim.api.nvim_buf_set_keymap(0, 'n', '<localleader>lt', '<cmd>call vimtex#fzf#run()<CR>', { silent = true })

vim.api.nvim_buf_set_keymap(0, 'n', '<localleader>lf', '<cmd>!latexindent -m -l -<CR>', { silent = true })
