vim.api.nvim_create_autocmd('ColorScheme', {
  pattern = '*',
  callback = function()
    local hl = vim.api.nvim_set_hl

    hl(0, 'EyelinerPrimary', { bold = true, underline = true })
    hl(0, 'EyelinerSecondary', { underline = true })

    -- curly underline of misspelled words
    hl(0, 'SpellBad', { undercurl = true, sp = '#e86671' })
    hl(0, 'SpellCap', { undercurl = true, sp = '#e5c07b' })
    hl(0, 'SpellLocal', { undercurl = true, sp = '#e5c07b' })
    hl(0, 'SpellRare', { undercurl = true, sp = '#e5c07b' })
    -- hi SpellRare  cterm=undercurl gui=undercurl ctermfg=NONE guifg=NONE guisp=#e5c07b

  end,
})

require('onenord').setup {
  styles = {
    comments = "italic",
    diagnostics = "undercurl",
  },
}
