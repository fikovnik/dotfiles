return {
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    branch = "main",
    init = function()
      -- Disable entire built-in ftplugin mappings to avoid conflicts.
      -- See https://github.com/neovim/neovim/tree/master/runtime/ftplugin for built-in ftplugins.
      vim.g.no_plugin_maps = true
    end,
    config = function()
      require("nvim-treesitter-textobjects").setup({
        select = {
          lookahead = true,
          selection_modes = {
            ["@parameter.outer"] = "v",
            ["@function.outer"] = "V",
            ["@code_block.inner"] = "V",
          },
        },
      })

      -- keymaps
      vim.keymap.set({ "x", "o" }, "af", function()
        require("nvim-treesitter-textobjects.select").select_textobject("@function.outer", "textobjects")
      end)
      vim.keymap.set({ "x", "o" }, "if", function()
        require("nvim-treesitter-textobjects.select").select_textobject("@function.inner", "textobjects")
      end)
      vim.keymap.set({ "x", "o" }, "ac", function()
        require("nvim-treesitter-textobjects.select").select_textobject("@class.outer", "textobjects")
      end)
      vim.keymap.set({ "x", "o" }, "ic", function()
        require("nvim-treesitter-textobjects.select").select_textobject("@class.inner", "textobjects")
      end)
      vim.keymap.set({ "x", "o" }, "aC", function()
        require("nvim-treesitter-textobjects.select").select_textobject("@block.outer", "textobjects")
      end)
      vim.keymap.set({ "x", "o" }, "iC", function()
        require("nvim-treesitter-textobjects.select").select_textobject("@block.inner", "textobjects")
      end)
    end,
  },
  -- {
  --   "nvim-treesitter/nvim-treesitter-textobjects",
  --   opts = {
  --     textobjects = {
  --       select = {
  --         enable = true,
  --         lookahead = true,
  --         keymaps = {
  --           ["aa"] = "@parameter.outer",
  --           ["ia"] = "@parameter.inner",
  --           ["af"] = "@function.outer",
  --           ["if"] = "@function.inner",
  --           ["ac"] = "@class.outer",
  --           ["ic"] = "@class.inner",
  --           ["iC"] = "@code_block.inner",
  --           ["aC"] = "@code_block.outer",
  --         },
  --       },
  --       swap = {
  --         enable = true,
  --         swap_next = {
  --           ["<M->>"] = "@parameter.inner",
  --         },
  --         swap_previous = {
  --           ["<M-<>"] = "@parameter.inner",
  --         },
  --       },
  --     },
  --   },
  -- },
}
