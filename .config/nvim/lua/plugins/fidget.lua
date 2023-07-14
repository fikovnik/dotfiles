return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "j-hui/fidget.nvim",
      tag = "legacy",
      opts = {
        text = { spinner = "dots" },
      },
    },
  },
}
