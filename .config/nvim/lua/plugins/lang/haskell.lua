local Util = require("util")

return {
  -- add to treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "haskell" })
      end
    end,
  },

  -- setup the LSP server
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      { "mrcjkb/haskell-tools.nvim", version = "1.x.x", config = true },
    },
    opts = {
      servers = {
        hls = {
          keys = {
            { "<localleader>a", function() require("haskell-tools").lsp.buf_eval_all() end, desc = "Eval all (Haskell)" },
          },
        },
      },
    },
  },
}
