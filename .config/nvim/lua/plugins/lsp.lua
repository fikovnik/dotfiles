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
    end,
  },

  {
    "neovim/nvim-lspconfig",
    keys = {
      { "<localleader>?", "<cmd>RustOpenExternalDocs<cr>", desc = "Docs (Rust)" },
      { "<localleader>R", "<cmd>RustReloadWorkspace<cr>", desc = "Reload workspace (Rust)" },
      { "<localleader>C", "<cmd>RustOpenCargo<cr>", desc = "Open Cargo (Rust)" },
    },
    opts = {
      setup = {
        ["rust-analyzer"] = function(_, opts)
          opts.highlightRelated.references = { enable = false }
        end,
      },
    },
  },

  {
    "j-hui/fidget.nvim",
    config = true,
  },
}
