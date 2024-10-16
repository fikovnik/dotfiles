return {
  "neovim/nvim-lspconfig",
  opts = {
    servers = {
      clangd = {
        mason = vim.fn.executable("clangd") == 0,
      },
    },
  },
}
