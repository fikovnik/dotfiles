require('rust-tools').setup {
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
