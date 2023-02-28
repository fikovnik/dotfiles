return {
  -- add to treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "c", "cpp" })
      end
    end,
  },

  -- correctly setup lspconfig for clangd
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        clangd = {},
      },
    },
  },
}
