local Util = require("util")

return {
  -- add to treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "r" })
      end
    end,
  },

  -- setup the LSP server
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        r_language_server = {},
      },
    },
  },
}
