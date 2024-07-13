require("lazyvim.util").lsp.on_attach(function(_, _)
  vim.opt_local.foldmethod = "expr"
  vim.opt_local.foldexpr = "nvim_treesitter#foldexpr()"
end)

return {
  {
    "neovim/nvim-lspconfig",
    init = function()
      local keys = require("lazyvim.plugins.lsp.keymaps").get()
      keys[#keys + 1] = { "<M-CR>", vim.lsp.buf.code_action, mode = { "n", "i" }, desc = "Actions" }
      keys[#keys + 1] = {
        "<M-p>",
        vim.lsp.buf.signature_help,
        mode = { "n", "i" },
        desc = "Signature Help",
        has = "signatureHelp",
      }
    end,
    opts = {
      setup = {
        rust_analyzer = function()
          -- prevent incompatibility issies with rust integartion
          return true
        end,
      },
    },
  },

  {
    "j-hui/fidget.nvim",
    config = true,
  },

  {
    "mrcjkb/rustaceanvim",
    opts = {
      server = {
        -- stylua: ignore
        on_attach = function(_, bufnr)
          vim.keymap.set("n", "<leader>cm", function() vim.cmd.RustLsp("expandMacro") end, { desc = "Expand Macro (Rust)", buffer = bufnr })
          vim.keymap.set("n", "<leader>co", function() vim.cmd.RustLsp("openDocs") end, { desc = "Open Docs (Rust)", buffer = bufnr })
          vim.keymap.set("n", "<leader>cC", function() vim.cmd.RustLsp("openCargo") end, { desc = "Open Cargo.toml (Rust)", buffer = bufnr })
        end,
        default_settings = {
          ["rust-analyzer"] = {
            inlayHints = {
              parameterHints = {
                enable = false,
              },
              typeHints = {
                enable = false,
              },
            },
            highlightRelated = {
              references = {
                enable = false,
              },
            },
          },
        },
      },
    },
  },
}
