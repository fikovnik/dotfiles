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
      dashboard = { enabled = false },
      scroll = { enabled = false },
      indent = { enabled = false },
    },
  },
}
