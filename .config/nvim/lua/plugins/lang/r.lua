local Util = require("util")

local function set_keymap(_, buffer) end

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
      setup = {
        r_language_server = function(_, opts)
          require("util").on_attach(function(client, buffer)
            if client.name == "r_language_server" then
              set_keymap(client, buffer)
            end
          end)
          return false
        end,
      },
    },
  },
}
