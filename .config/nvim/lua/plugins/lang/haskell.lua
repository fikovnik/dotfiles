local Util = require("util")

local function set_keymap(_, buffer)
  local ht = require("haskell-tools")
  vim.keymap.set("n", "<localleader>a", ht.lsp.buf_eval_all, { buffer = buffer, desc = "Eval all (Haskell)" })
end

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

  -- setup mason lsp / dap extensions
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "haskell-language-server" })
      end
    end,
  },

  -- setup the LSP server
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      { "mrcjkb/haskell-tools.nvim", version = "1.x.x" },
    },
    opts = {
      servers = {
        hls = {},
      },
      setup = {
        hls = function(_, opts)
          Util.on_attach(function(client, buffer)
            if client.name == "haskell-tools.nvim" then
              set_keymap(client, buffer)
            end
          end)

          require("haskell-tools").setup({})
          return true
        end,
      },
    },
  },
}
