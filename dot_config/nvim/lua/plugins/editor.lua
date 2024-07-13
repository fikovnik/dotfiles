return {
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "sindrets/diffview.nvim",
    },
    keys = {
      { "<leader>gg", "<cmd>Neogit<cr>", desc = "Status" },
    },
    cmd = {
      "Neogit",
    },
    opts = {
      disable_hint = true,
      signs = {
        section = { "", "" },
        item = { "", "" },
        hunk = { "", "" },
      },
      integrations = {
        diffview = true,
      },
    },
  },

  {
    "sindrets/diffview.nvim",
    keys = {
      { "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "Diffview" },
      { "<leader>gH", "<cmd>DiffviewFileHistory<cr>", desc = "History" },
    },
    cmd = {
      "DiffviewOpen",
      "DiffviewFileHistory",
    },
    opts = {
      use_icons = false,
      view = {
        merge_tool = {
          layout = "diff3_mixed",
        },
      },
    },
  },

  {
    "nvim-telescope/telescope.nvim",
    keys = {
      -- disable git mappings
      { "<leader>gc", false },
      { "<leader>gs", false },
    },
    opts = function(_, opts)
      opts.defaults.mappings.i["<C-h>"] = "results_scrolling_left"
      opts.defaults.mappings.i["<C-l>"] = "results_scrolling_right"
      opts.defaults.mappings.n["h"] = "results_scrolling_left"
      opts.defaults.mappings.n["l"] = "results_scrolling_right"
    end,
  },

  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin",
    },
  },

  {
    "folke/which-key.nvim",
    opts = {
      spec = {
        ["<leader>d"] = { name = "+debug" },
      },
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
    "ntpeters/vim-better-whitespace",
    event = "VeryLazy",
    config = function()
      vim.g.better_whitespace_enabled = 0
      vim.g.strip_only_modified_lines = 1
      vim.g.strip_whitespace_on_save = 1
      vim.g.strip_whitespace_confirm = 0
    end,
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
    "folke/flash.nvim",
    opts = {
      label = {
        current = false,
      },
      modes = {
        search = {
          enabled = false,
        },
      },
    },
  },

  {
    "echasnovski/mini.align",
    version = false,
    config = true,
  },
}
