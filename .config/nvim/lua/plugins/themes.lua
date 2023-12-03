return {
  {
    "rmehri01/onenord.nvim",
    config = function()
      local colors = require("onenord.colors").load()
      require("onenord").setup({
        styles = {
          comments = "italic",
          diagnostics = "undercurl",
        },
        custom_highlights = {
          TelescopeSelection = { bg = colors.highlight_dark, fg = "NONE" },
          WinBar = { bg = colors.highlight_dark },
          WinBarNC = { bg = colors.highlight_dark },
          FlashLabel = { fg = colors.pink, style = "bold,underline" },
          DiffText = { bg = "#fd5da8", fg = "#000000" },
          CodeBlock = { bg = "#1a1b26" },
        },
      })
    end,
  },

  {
    "catppuccin/nvim",
    name = "catppuccin",
    lazy = true,
    opts = {
      flavour = "frappe",
      background = {
        light = "latte",
        dark = "frappe",
      },
      integrations = {
        fidget = true,
        lsp_saga = true,
        native_lsp = {
          enabled = true,
          virtual_text = {
            errors = {},
            hints = {},
            warnings = {},
            information = {},
          },
          underlines = {
            errors = { "undercurl" },
            hints = { "undercurl" },
            warnings = { "undercurl" },
            information = { "undercurl" },
          },
        },
      },
    },
  },
}
