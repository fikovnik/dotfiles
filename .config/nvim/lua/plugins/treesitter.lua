return {
  {
    "nvim-treesitter/nvim-treesitter",
    version = false, -- last release is way too old and doesn't work on Windows
    dependencies = {
      -- "nvim-treesitter/nvim-treesitter-context",
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    build = ":TSUpdate",
    event = { "BufReadPost", "BufNewFile" },
    keys = {
      { "<M-=>", desc = "Increment selection", mode = "x" },
      { "<M-->", desc = "Decrement selection", mode = "x" },
    },
    opts = {
      highlight = { enable = true },
      indent = { enable = true },
      ensure_installed = {
        "bash",
        "vimdoc",
        "html",
        "json",
        "lua",
        "markdown",
        "markdown_inline",
        "query",
        "regex",
        "vim",
        "yaml",
      },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<M-=>",
          node_incremental = "<M-=>",
          scope_incremental = "<nop>",
          node_decremental = "<M-->",
        },
      },
      textobjects = {
        select = {
          enable = true,
          lookahead = true,
          keymaps = {
            ["af"] = { query = "@function.outer", desc = "Around function" },
            ["if"] = { query = "@function.inner", desc = "In function" },
            ["ab"] = { query = "@block.outer", desc = "Around block" },
            ["ib"] = { query = "@block.inner", desc = "In block" },
            ["ap"] = { query = "@parameter.outer", desc = "Around parameter" },
            ["ip"] = { query = "@parameter.inner", desc = "In parameter" },
          },
        },
        swap = {
          enable = true,
          swap_next = {
            ["<M->>"] = "@parameter.inner",
          },
          swap_previous = {
            ["<M-<>"] = "@parameter.inner",
          },
        },
      },
    },
    ---@type TSConfig
    ---@param opts TSConfig
    config = function(_, opts)
      require("nvim-treesitter.configs").setup(opts)
    end,
  },
}
