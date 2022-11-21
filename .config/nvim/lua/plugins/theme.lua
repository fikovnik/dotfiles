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

    hl(0, 'CmpBorder', { fg = '#6c7a96', bg = '#3b4252' })
  end,
})

local colors = require('onenord.colors').load()
require('onenord').setup {
  styles = {
    comments = "italic",
    diagnostics = "undercurl",
  },
  custom_highlights = {
    TelescopeSelection = { bg = colors.highlight_dark, fg = "NONE" },
    WinBar = { bg = colors.highlight_dark },
    WinBarNC = { bg = colors.highlight_dark },
    NavicIconsFile = { bg = colors.highlight_dark, fg = colors.blue },
    NavicIconsModule = { bg = colors.highlight_dark, fg = colors.blue },
    NavicIconsNamespace = { bg = colors.highlight_dark, fg = colors.yellow },
    NavicIconsPackage = { bg = colors.highlight_dark, fg = colors.orange },
    NavicIconsClass = { bg = colors.highlight_dark, fg = colors.yellow },
    NavicIconsMethod = { bg = colors.highlight_dark, fg = colors.purple },
    NavicIconsProperty = { bg = colors.highlight_dark, fg = colors.blue },
    NavicIconsField = { bg = colors.highlight_dark, fg = colors.blue },
    NavicIconsConstructor = { bg = colors.highlight_dark, fg = colors.yellow },
    NavicIconsEnum = { bg = colors.highlight_dark, fg = colors.yellow },
    NavicIconsInterface = { bg = colors.highlight_dark, fg = colors.yellow },
    NavicIconsFunction = { bg = colors.highlight_dark, fg = colors.purple },
    NavicIconsVariable = { bg = colors.highlight_dark, fg = colors.blue },
    NavicIconsConstant = { bg = colors.highlight_dark, fg = colors.orange },
    NavicIconsString = { bg = colors.highlight_dark, fg = colors.green },
    NavicIconsNumber = { bg = colors.highlight_dark, fg = colors.orange },
    NavicIconsBoolean = { bg = colors.highlight_dark, fg = colors.orange },
    NavicIconsArray = { bg = colors.highlight_dark, fg = colors.yellow },
    NavicIconsObject = { bg = colors.highlight_dark, fg = colors.orange },
    NavicIconsKey = { bg = colors.highlight_dark, fg = colors.purple },
    NavicIconsNull = { bg = colors.highlight_dark, fg = colors.red },
    NavicIconsEnumMember = { bg = colors.highlight_dark, fg = colors.cyan },
    NavicIconsStruct = { bg = colors.highlight_dark, fg = colors.yellow },
    NavicIconsEvent = { bg = colors.highlight_dark, fg = colors.purple },
    NavicIconsOperator = { bg = colors.highlight_dark, fg = colors.purple },
    NavicIconsTypeParameter = { bg = colors.highlight_dark, fg = colors.yellow },
    NavicText = { bg = colors.highlight_dark, fg = colors.fg },
    NavicSeparator = { bg = colors.highlight_dark, fg = colors.cyan },
  },
}
