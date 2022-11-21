local status_ok, neotree = pcall(require, 'neo-tree')
if not status_ok then return end

neotree.setup {
  close_if_last_window = false,
  popup_border_style = 'single',
  source_selector = {
    winbar = true,
    content_layout = 'center',
  },
  default_component_configs = {
    indent = {
      with_markers = true,
      indent_marker = '│',
      last_indent_marker = '└',
      indent_size = 2,
    },
    icon = {
      folder_closed = '',
      folder_open = '',
      folder_empty = 'ﰊ',
      default = ' ',
    },
  },
  window = {
    width = 30,
    mappings = {
      ['<M-CR>'] = function(state)
        state.commands['open'](state)
        vim.cmd('Neotree close')
      end,
      ['<tab>'] = function(state)
        local node = state.tree:get_node()
        if require('neo-tree.utils').is_expandable(node) then
          state.commands['toggle_node'](state)
        else
          state.commands['open'](state)
          vim.cmd('Neotree reveal')
        end
      end,
    },
  },
  filesystem = {
    follow_current_file = true,
    hijack_netrw_behavior = 'open_current',
    use_libuv_file_watcher = true,
  },
  event_handlers = {
    { event = 'neo_tree_buffer_enter', handler = function(_) vim.opt_local.signcolumn = 'auto' end },
  },
}
