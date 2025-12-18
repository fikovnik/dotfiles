return {
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    opts = function(_, opts)
      table.remove(opts.sections.lualine_z)
    end,
  },
  {
    "snacks.nvim",
    opts = {
      statuscolumn = { enabled = true },
      dashboard = { enabled = false },
      scroll = { enabled = false },
      indent = { enabled = false },
    },
  },
  {
    "catppuccin/nvim",
    opts = {
      custom_highlights = function(colors)
        return {
          DiagnosticDeprecated = { undercurl = true, strikethrough = false },
        }
      end,
    },
  },
}
