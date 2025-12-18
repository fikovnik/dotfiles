return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        ["*"] = {
          keys = {
            { "<M-CR>", vim.lsp.buf.code_action, mode = { "n", "i" }, desc = "Code actions" },
          },
        },
      },
    },
  },

  {
    "j-hui/fidget.nvim",
    config = true,
  },
}
