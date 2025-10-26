return {
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin",
    },
  },

  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function(_, opts)
      vim.keymap.set("n", "-", require("oil").open, { desc = "Open parent directory" })
      require("oil").setup(opts)
    end,
    opts = {
      columns = {
        "icon",
        "permissions",
        "size",
        "mtime",
      },
    },
  },

  {
    "folke/trouble.nvim",
    opts = {
      use_diagnostic_signs = true,
      auto_preview = false,
    },
  },

  {
    "gbprod/yanky.nvim",
    opts = function(_, opts)
      opts.system_clipboard = {
        sync_with_ring = false,
      }
    end,
  },

  {
    "nvim-mini/mini.align",
    version = false,
    config = true,
  },

  {
    "saghen/blink.cmp",
    opts = {
      completion = {
        list = { selection = { preselect = false, auto_insert = false } },
      },
    },
  },
}
