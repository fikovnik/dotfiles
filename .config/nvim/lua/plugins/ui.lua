local Util = require("util")

return {
  {
    "stevearc/dressing.nvim",
    event = "VeryLazy",
    -- lazy = true,
    -- init = function()
    --   ---@diagnostic disable-next-line: duplicate-set-field
    --   vim.ui.select = function(...)
    --     require("lazy").load({ plugins = { "dressing.nvim" } })
    --     return vim.ui.select(...)
    --   end
    --   -- ---@diagnostic disable-next-line: duplicate-set-field
    --   vim.ui.input = function(...)
    --     require("lazy").load({ plugins = { "dressing.nvim" } })
    --     return vim.ui.input(...)
    --   end
    -- end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    event = { "BufReadPost", "BufNewFile" },
    cmd = { "IBLToggle" },
    keys = {
      {
        "<leader>ti",
        Util.cmd("IBLToggle"),
        desc = "Indent Guide",
      },
    },
    opts = {
      enabled = false,
      indent = {
        char = "│",
      },
      whitespace = {
        remove_blankline_trail = true,
      },
      exclude = {
        filetypes = { "help", "alpha", "dashboard", "neo-tree", "Trouble", "lazy" },
      },
    },
  },
}
