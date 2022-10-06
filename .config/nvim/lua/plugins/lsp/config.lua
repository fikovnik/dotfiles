local lsp = require('lspconfig')
local keybinds = require('keybinds')
local utils = require('plugins.lsp.utils')

M = {}

M.capabilities = vim.lsp.protocol.make_client_capabilities()
require('cmp_nvim_lsp').update_capabilities(M.capabilities)

M.on_attach = function(client, buf)
  local navic = require('nvim-navic')
  navic.attach(client, buf)

  if navic.is_available() then
    vim.o.winbar = "%{%v:lua.require'nvim-navic'.get_location()%}"
  end

  utils.fmt_on_save(client, buf)
  keybinds.set_lsp_integration(buf)

  local aerial_avail, aerial = pcall(require, "aerial")
  if aerial_avail then
    aerial.on_attach(client, buf)
  end
end

local flags = {
  allow_incremental_sync = true,
  debounce_text_changes = 200,
}

vim.diagnostic.config({
  virtual_text = {
    source = 'always',
  },
  float = {
    source = 'always',
  },
})

local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, 'lua/?.lua')
table.insert(runtime_path, 'lua/?/init.lua')

lsp.sumneko_lua.setup({
  flags = flags,
  capabilities = M.capabilities,
  on_attach = M.on_attach,
  settings = {
    Lua = {
      completion = {
        enable = true,
        showWord = 'Disable',
      },
      runtime = {
        version = 'LuaJIT',
        path = runtime_path,
      },
      diagnostics = {
        globals = { 'vim' },
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file('', true),
      },
      telemetry = {
        enable = false,
      },
    },
  },
})

return M
