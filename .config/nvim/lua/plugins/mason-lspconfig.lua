local present, mason_lspconfig = pcall(require, "mason-lspconfig")
if not present then return end

mason_lspconfig.setup {
  ensure_installed = vim.g.my.lsp_servers,
}
