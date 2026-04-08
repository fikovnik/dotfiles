return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "asm",
      },
    },
  },

  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        asm_lsp = {},
      },
    },
  },
}
