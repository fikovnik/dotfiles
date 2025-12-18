return {
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin",
    },
  },

  {
    "stevearc/oil.nvim",
    keys = {
      { "-", "<cmd>Oil<cr>", desc = "Open parent directory" },
    },
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

  {
    "saghen/blink.cmp",
    dependencies = {
      "erooke/blink-cmp-latex",
    },

    opts = {
      sources = {
        default = { "latex" },

        providers = {
          latex = {
            name = "Latex",
            module = "blink-cmp-latex",
            opts = {
              -- set to true to insert the latex command instead of the symbol
              insert_command = false,
            },
          },
        },
      },
    },
  },
}
