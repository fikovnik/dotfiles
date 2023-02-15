return {
  {
    "nvim-treesitter/nvim-treesitter",
    version = false, -- last release is way too old and doesn't work on Windows
    build = ":TSUpdate",
    event = { "BufReadPost", "BufNewFile" },
    keys = {
      { "<M-=>", desc = "Increment selection", mode = "x" },
      { "<M-->", desc = "Decrement selection", mode = "x" },
    },
    opts = {
      highlight = { enable = true },
      indent = { enable = true },
      context_commentstring = { enable = true, enable_autocmd = false },
      ensure_installed = {
        "bash",
        "help",
        "html",
        "javascript",
        "json",
        "lua",
        "markdown",
        "markdown_inline",
        "python",
        "query",
        "regex",
        "rust",
        "tsx",
        "typescript",
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
