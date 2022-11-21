local status_ok, aerial = pcall(require, 'aerial')
if not status_ok then return end

aerial.setup {
  layout = {
    min_width = 20,
    placement = "edge",
  },
  attach_mode = "global",
  show_guides = true,
  filter_kind = false,
}
