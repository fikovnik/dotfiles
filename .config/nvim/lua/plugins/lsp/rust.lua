local extension_path = vim.env.HOME .. '/.vscode-server/extensions/vadimcn.vscode-lldb-1.7.4/'
local codelldb_path = extension_path .. 'adapter/codelldb'
local liblldb_path = extension_path .. 'lldb/lib/liblldb.so'

local opts = {
  dap = {
    adapter = require('rust-tools.dap').get_codelldb_adapter(codelldb_path, liblldb_path)
  },
  tools = {
    inlay_hints = {
      show_parameter_hints = false,
    },
  },
  server = {
    capabilities = require('plugins.lsp.config').capabilities,
    on_attach = require('plugins.lsp.config').on_attach,
    settings = {
      -- https://github.com/rust-lang/rust-analyzer/blob/master/crates/rust-analyzer/src/config.rs
      ["rust-analyzer"] = {
        cargo = {
          allFeatures = true,
        },
        workspace = {
          symbol = {
            search = {
              scope = "workspace_and_dependencies"
            },
          },
        },
      },
    },
  },
}

require('rust-tools').setup(opts)
