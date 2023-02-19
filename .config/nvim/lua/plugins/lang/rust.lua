local Util = require("util")

local function set_keymap(_, buffer)
  vim.keymap.set("n", "<localleader>C", Util.cmd("RustOpenCargo"), { buffer = buffer, desc = "Open Cargo.toml" })
  vim.keymap.set(
    "n",
    "<localleader>wr",
    Util.cmd("RustReloadWorkspace"),
    { buffer = buffer, desc = "Reload workspace" }
  )
  vim.keymap.set("n", "<localleader>?", Util.cmd("RustOpenExternalDocs"), { buffer = buffer, desc = "Docs" })
  vim.keymap.set("n", "<M-CR>", Util.cmd("RustCodeAction"), { buffer = buffer, desc = "Code action (Rust)" })
  -- vim.keymap.set("n", "<leader>ct", "<CMD>RustDebuggables<CR>", { buffer = buffer, desc = "Run Test" })

  vim.keymap.set("n", "K", function()
    local winid = require("ufo").peekFoldedLinesUnderCursor()
    if not winid then
      require("rust-tools").hover_actions.hover_actions()
    end
  end, { buffer = buffer, desc = "Hover actions (Rust)" })
end

return {
  -- add to treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "rust", "toml" })
      end
    end,
  },

  -- setup mason lsp / dap extensions
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "codelldb", "rust-analyzer" })
      end
    end,
  },

  -- setup the LSP server
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "simrat39/rust-tools.nvim",
    },
    opts = {
      servers = {
        -- make sure mason installs the server
        rust_analyzer = {},
      },
      setup = {
        rust_analyzer = function(_, opts)
          require("util").on_attach(function(client, buffer)
            if client.name == "rust_analyzer" then
              set_keymap(client, buffer)
            end
          end)

          -- rust tools configuration for debugging support
          local mason_registry = require("mason-registry")
          local codelldb = mason_registry.get_package("codelldb")
          local extension_path = codelldb:get_install_path() .. "/extension/"
          local codelldb_path = extension_path .. "adapter/codelldb"
          local liblldb_path = vim.fn.has("mac") == 1 and extension_path .. "lldb/lib/liblldb.dylib"
            or extension_path .. "lldb/lib/liblldb.so"

          local rust_tools_opts = vim.tbl_deep_extend("force", opts, {
            dap = {
              adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path),
            },
            tools = {
              inlay_hints = {
                show_parameter_hints = false,
                other_hints_prefix = "",
              },
            },
            server = {
              settings = {
                ["rust-analyzer"] = {
                  cargo = {
                    allFeatures = true,
                    loadOutDirsFromCheck = true,
                    runBuildScripts = true,
                  },
                  -- workspace = {
                  --   symbol = {
                  --     search = {
                  --       -- scope = 'workspace_and_dependencies',
                  --       scope = "workspace",
                  --     },
                  --   },
                  -- },
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
          })
          require("rust-tools").setup(rust_tools_opts)
          return true
        end,
      },
    },
  },
}
