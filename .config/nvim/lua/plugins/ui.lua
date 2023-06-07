return {
  {
    "stevearc/dressing.nvim",
    lazy = true,
    init = function()
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.select = function(...)
        require("lazy").load({ plugins = { "dressing.nvim" } })
        return vim.ui.select(...)
      end
      -- ---@diagnostic disable-next-line: duplicate-set-field
      -- vim.ui.input = function(...)
      --   require("lazy").load({ plugins = { "dressing.nvim" } })
      --   return vim.ui.input(...)
      -- end
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPost", "BufNewFile" },
    keys = {
      {
        "<leader>ti",
        function()
          require("indent_blankline.commands").toggle()
        end,
        desc = "Indent Guide",
      },
    },
    init = function()
      vim.g.indent_blankline_enabled = false
    end,
    opts = {
      char = "│",
      filetype_exclude = { "help", "alpha", "dashboard", "neo-tree", "Trouble", "lazy" },
      show_trailing_blankline_indent = false,
      show_current_context = false,
    },
  },
  -- {
  --   "lukas-reineke/headlines.nvim",
  --   ft = { "markdown", "rmd", "org" },
  --   dependencies = "nvim-treesitter/nvim-treesitter",
  --   opts = {},
  -- },
}
