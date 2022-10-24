local status_ok, trouble = pcall(require, 'trouble')
if not status_ok then return end

trouble.setup {
  icons = false,
  use_diagnostic_signs = true,
  auto_jump = { 'lsp_definitions', 'lsp_references', 'lsp_implementations', 'lsp_type_definitions' },
}
