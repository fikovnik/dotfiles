vim.api.nvim_create_autocmd('ColorScheme', {
  pattern = '*',
  callback = function()
    local colors = require('onenord.colors').load()
    local hl = vim.api.nvim_set_hl

    hl(0, 'EyelinerPrimary', { link = 'PounceAcceptBest' })
    hl(0, 'EyelinerSecondary', { link = 'PounceAccept' })

    hl(0, 'SpellBad', { undercurl = true, sp = '#e86671' })
    hl(0, 'SpellCap', { undercurl = true, sp = '#e5c07b' })
    hl(0, 'SpellLocal', { undercurl = true, sp = '#e5c07b' })
    hl(0, 'SpellRare', { undercurl = true, sp = '#e5c07b' })

    hl(0, 'QuickFixLine', { bold = true, italic = false })

    hl(0, 'WinBar', { bg = colors.highligh_dark })

    hl(0, 'CmpBorder', { fg = '#6c7a96', bg = '#3b4252' })
  end,
})

require('onenord').setup {
  styles = {
    comments = "italic",
    diagnostics = "undercurl",
  },
}
