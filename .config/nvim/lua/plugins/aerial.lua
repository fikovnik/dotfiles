local status_ok, aerial = pcall(require, 'aerial')
if not status_ok then return end

aerial.setup {
  backends = { 'lsp', 'treesitter', 'markdown' },
  layout = {
    min_width = 20,
  },
  show_guides = true,
  filter_kind = false,
}
