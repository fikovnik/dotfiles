local Util = require("util")

return {
  {
    "echasnovski/mini.bufremove",
    -- stylua: ignore
    keys = {
      { "<leader>bd", function() require("mini.bufremove").delete(0, false) end, desc = "Delete Buffer" },
      { "<leader>bD", function() require("mini.bufremove").delete(0, true) end,  desc = "Delete Buffer (Force)" },
    },
  },
  {
    "echasnovski/mini.move",
    event = "VeryLazy",
    opts = {
      mappings = {
        -- move visual selection in Visual mode
        left = "<M-Left>",
        right = "<M-Rigt>",
        down = "<M-Down>",
        up = "<M-Up>",
        -- move current line in Normal mode
        line_left = "<M-Left>",
        line_right = "<M-Right>",
        line_down = "<M-Down>",
        line_up = "<M-Up>",
      },
    },
    config = function(_, opts)
      require("mini.move").setup(opts)
    end,
  },
  {
    "echasnovski/mini.align",
    event = "VeryLazy",
    opts = {
      mappings = {
        start = "<leader>a",
        start_with_preview = "<leader>A",
      },
    },
    config = function(_, opts)
      require("mini.align").setup(opts)
    end,
  },
  {
    "windwp/nvim-spectre",
    keys = {
      -- stylua: ignore
      { "<leader>sr", function() require("spectre").open() end, desc = "Replace in files", },
    },
  },
  {
    "folke/trouble.nvim",
    cmd = { "TroubleToggle", "Trouble" },
    opts = {
      use_diagnostic_signs = true,
      icons = false,
    },
    keys = {
      { "<leader>od", Util.cmd("TroubleToggle document_diagnostics"),  desc = "Document Diagnostics" },
      { "<leader>oD", Util.cmd("TroubleToggle workspace_diagnostics"), desc = "Workspace Diagnostics" },
      { "<leader>ol", Util.cmd("TroubleToggle loclist"),               desc = "Location List" },
      { "<leader>oq", Util.cmd("TroubleToggle quickfix"),              desc = "Quickfix List" },
    },
  },
  {
    "tpope/vim-repeat",
    event = "VeryLazy",
  },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "AckslD/nvim-neoclip.lua",
      event = "VeryLazy",
      opts = {
        keys = {
          telescope = {
            i = {
              select = "<M-CR>",
              paste = "<CR>",
              paste_behind = "<S-CR>",
              replay = "<C-q>",
              delete = "<C-d>",
            },
          },
        },
        filter = function(data)
          return not Util.all(data.event.regcontents, Util.is_whitespace)
        end,
      },
      config = function(_, opts)
        vim.keymap.set({ "n", "x", "i" }, "<M-y>", Util.cmd("Telescope neoclip"), { desc = "Neoclip" })
        require("neoclip").setup(opts)
        require("telescope").load_extension("neoclip")
      end,
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
    "mbbill/undotree",
    cmd = "UndotreeToggle",
    keys = {
      { "<leader>eu", Util.cmd("silent! %foldopen! | UndotreeToggle | UndotreeFocus"), desc = "Undo" },
    },
  },
  {
    "kylechui/nvim-surround",
    event = "VeryLazy",
    config = true,
  },
  {
    "mg979/vim-visual-multi",
    event = "VeryLazy",
  },
  {
    "rlane/pounce.nvim",
    cmd = { "Pounce", "PounceRepeat" },
  },
}
