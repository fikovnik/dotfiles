local lsp = require('lspconfig')
local keybinds = require('keybinds')
local utils = require('plugins.lsp.utils')

local float_style = {
  focusable = true,
  style = 'minimal',
  border = 'single',
}

M = {}

M.capabilities = vim.lsp.protocol.make_client_capabilities()
require('cmp_nvim_lsp').default_capabilities(M.capabilities)

vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, float_style)
vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, float_style)

M.on_attach = function(client, buf)
  local navic = require('nvim-navic')
  navic.attach(client, buf)

  if navic.is_available() then
    vim.o.winbar = "%{%v:lua.require'nvim-navic'.get_location()%}"
  end

  utils.fmt_on_save(client, buf)
  keybinds.set_lsp_integration(buf)
end

local flags = {
  allow_incremental_sync = true,
  debounce_text_changes = 200,
}

vim.diagnostic.config({
  severity_sort = true,
  virtual_text = {
    source = 'if_many',
  },
  float = {
    source = 'if_many',
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
