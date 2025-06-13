return {
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
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

  {
    "ibhagwan/fzf-lua",
    optional = true,
    opts = function(_, opts)
      local actions = require("fzf-lua.actions")
      opts.files = {
        actions = {
          ["ctrl-h"] = actions.toggle_hidden,
          ["ctrl-i"] = { actions.toggle_ignore },
        },
      }
      opts.grep = {
        actions = {
          ["ctrl-h"] = actions.toggle_hidden,
          ["ctrl-i"] = { actions.toggle_ignore },
        },
      }
      return opts
    end,
  },
}
