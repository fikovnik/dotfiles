local Util = require("util")

return {
  -- Extend auto completion
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      {
        "Saecki/crates.nvim",
        event = { "BufRead Cargo.toml" },
        config = function(_, opts)
          require("crates").setup(opts)
          local function cmd(lhs, rhs)
            vim.api.nvim_create_user_command(lhs, "lua require('crates')." .. rhs, {})
          end
          cmd("CratesUpdate", "update()")
          cmd("CratesReload", "reload()")
          cmd("CratesHide", "hide()")
          cmd("CratesToggle", "toggle()")
          cmd("CratesUpdateCrate", "update_crate()")
          cmd("CratesUpdateCrates", "update_crates()")
          cmd("CratesUpdateAllCrates", "update_all_crates()")
          cmd("CratesUpgradeCrate", "upgrade_crate()")
          cmd("CratesUpgradeCrates", "upgrade_crates()")
          cmd("CratesUpgradeAllCrates", "upgrade_all_crates()")
          cmd("CratesShowPopup", "show_popup()")
          cmd("CratesShowVersionsPopup", "show_versions_popup()")
          cmd("CratesShowFeaturesPopup", "show_features_popup()")
          cmd("CratesFocusPopup", "focus_popup()")
          cmd("CratesHidePopup", "hide_popup()")
        end,
      },
    },
    opts = function(_, opts)
      local cmp = require("cmp")
      opts.sources = cmp.config.sources(vim.list_extend(opts.sources, {
        { name = "crates" },
      }))
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "ron", "rust", "toml" })
      end
    end,
  },

  -- Ensure Rust debugger is installed
  {
    "williamboman/mason.nvim",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "codelldb" })
      end
    end,
  },

  {
    "simrat39/rust-tools.nvim",
    lazy = true,
    opts = function()
      local ok, mason_registry = pcall(require, "mason-registry")
      local adapter ---@type any
      if ok then
        -- rust tools configuration for debugging support
        local codelldb = mason_registry.get_package("codelldb")
        local extension_path = codelldb:get_install_path() .. "/extension/"
        local codelldb_path = extension_path .. "adapter/codelldb"
        local liblldb_path = vim.fn.has("mac") == 1 and extension_path .. "lldb/lib/liblldb.dylib"
          or extension_path .. "lldb/lib/liblldb.so"
        adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path)
      end
      return {
        dap = {
          adapter = adapter,
        },
        tools = {
          on_initialized = function()
            vim.cmd([[
                  augroup RustLSP
                    autocmd CursorHold                      *.rs silent! lua vim.lsp.buf.document_highlight()
                    autocmd CursorMoved,InsertEnter         *.rs silent! lua vim.lsp.buf.clear_references()
                    autocmd BufEnter,CursorHold,InsertLeave *.rs silent! lua vim.lsp.codelens.refresh()
                  augroup END
                ]])
          end,
          inlay_hints = {
            show_parameter_hints = false,
          },
          hover_actions = {
            auto_focus = true,
          },
        },
      }
    end,
    config = function() end,
  },

  -- Correctly setup lspconfig for Rust 🚀
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        -- Ensure mason installs the server
        rust_analyzer = {
          keys = {
            { "K", Util.cmd("RustHoverActions"), desc = "Hover Actions (Rust)" },
            { "<localleader>X", Util.cmd("RustDebuggables"), desc = "Debug (Rust)" },
            { "<localleader>x", Util.cmd("RustRunnables"), desc = "Run (Rust)" },
            { "<localleader>?", Util.cmd("RustOpenExternalDocs"), desc = "Docs (Rust)" },
            { "<localleader>wr", Util.cmd("RustReloadWorkspace"), desc = "Reload workspace (Rust)" },
            { "<localleader>C", Util.cmd("RustOpenCargo"), desc = "Open Cargo.toml (Rust)" },
          },
          settings = {
            ["rust-analyzer"] = {
              cargo = {
                features = "all",
                loadOutDirsFromCheck = true,
                runBuildScripts = true,
              },
              highlightRelated = {
                references = {
                  enable = false,
                },
              },
              -- Add clippy lints for Rust.
              checkOnSave = {
                allFeatures = true,
                command = "clippy",
                extraArgs = { "--no-deps" },
              },
              procMacro = {
                enable = true,
                ignored = {
                  ["async-trait"] = { "async_trait" },
                  ["napi-derive"] = { "napi" },
                  ["async-recursion"] = { "async_recursion" },
                },
              },
            },
          },
        },
        taplo = {
          keys = {
            {
              "K",
              function()
                if vim.fn.expand("%:t") == "Cargo.toml" and require("crates").popup_available() then
                  require("crates").show_popup()
                else
                  vim.lsp.buf.hover()
                end
              end,
              desc = "Show Crate Documentation",
            },
          },
        },
      },
      setup = {
        rust_analyzer = function(_, opts)
          local rust_tools_opts = Util.opts("rust-tools.nvim")
          require("rust-tools").setup(vim.tbl_deep_extend("force", rust_tools_opts or {}, { server = opts }))
          return true
        end,
      },
    },
  },
}
