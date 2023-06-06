local Util = require("util")

local function set_keymap(_, buffer)
  vim.keymap.set(
    "n",
    "<localleader>h",
    Util.cmd("ClangdSwitchSourceHeader"),
    { buffer = buffer, desc = "Switch to/from header" }
  )
end

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

  -- setup the LSP server
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        clangd = {
          cmd = {
            "clangd",
            "--offset-encoding=utf-16",
          },
        },
      },
      setup = {
        clangd = function(_, opts)
          require("util").on_attach(function(client, buffer)
            if client.name == "clangd" then
              set_keymap(client, buffer)
            end
          end)
          return false
        end,
      },
    },
  },
}
