return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = { "commonlisp", "scheme", "racket" },
    },
  },
  {
    "eraserhd/parinfer-rust",
    build = "cargo build --release",
    ft = { "lisp", "scheme", "racket" },
    cond = function()
      return vim.fn.executable("cargo") == 1
    end,
  },
  -- {
  --   "neovim/nvim-lspconfig",
  --   opts = {
  --     servers = {
  --       racket_langserver = {},
  --     },
  --   },
  -- },
}
