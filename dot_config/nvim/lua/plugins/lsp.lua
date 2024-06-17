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
      keys[#keys + 1] =
        { "<M-p>", vim.lsp.buf.signature_help, mode = { "n", "i" }, desc = "Signature Help", has = "signatureHelp" }
    end,
  },

  {
    "j-hui/fidget.nvim",
    config = true,
  },
}
