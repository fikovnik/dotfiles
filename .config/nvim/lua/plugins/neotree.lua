return {
  {
    "nvim-neo-tree/neo-tree.nvim",
    dependencies = { "MunifTanjim/nui.nvim" },
    cmd = "Neotree",
    keys = {
      {
        "<leader>fe",
        function()
          require("neo-tree.command").execute({ toggle = true, dir = require("util").get_root() })
        end,
        desc = "Explorer (root dir)",
      },
      {
        "<leader>fE",
        function()
          require("neo-tree.command").execute({ toggle = true, dir = vim.loop.cwd() })
        end,
        desc = "Explorer (cwd)",
      },
      {
        "<leader>be",
        function()
          require("neo-tree.command").execute({ toggle = true, source = "buffers" })
        end,
        desc = "Explorer",
      },
      {
        "<leader>ge",
        function()
          require("neo-tree.command").execute({ toggle = true, source = "git_status" })
        end,
        desc = "Explorer",
      },
    },
    deactivate = function()
      vim.cmd([[Neotree close]])
    end,
    init = function()
      vim.g.neo_tree_remove_legacy_commands = 1
      if vim.fn.argc() == 1 then
        local stat = vim.loop.fs_stat(vim.fn.argv(0))
        if stat and stat.type == "directory" then
          require("neo-tree")
        end
      end
    end,
    opts = {
      close_if_last_window = false,
      popup_border_style = "single",
      source_selector = {
        winbar = true,
        content_layout = "center",
      },
      default_component_configs = {
        indent = {
          with_markers = true,
          indent_marker = "│",
          last_indent_marker = "└",
          indent_size = 2,
        },
        icon = {
          folder_closed = "",
          folder_open = "",
          folder_empty = "ﰊ",
          default = " ",
        },
      },
      window = {
        mappings = {
          ["<space>"] = "none",
          ["P"] = { "toggle_preview", config = { use_float = false } },
          ["<M-CR>"] = function(state)
            state.commands["open"](state)
            vim.cmd("Neotree close")
          end,
          ["<tab>"] = function(state)
            local node = state.tree:get_node()
            if require("neo-tree.utils").is_expandable(node) then
              state.commands["toggle_node"](state)
            else
              state.commands["open"](state)
              vim.cmd("Neotree reveal")
            end
          end,
        },
      },
      buffers = {
        window = {
          mappings = {
            ["d"] = "buffer_delete",
            ["D"] = "delete",
          },
        },
      },
      filesystem = {
        bind_to_cwd = false,
        follow_current_file = true,
        use_libuv_file_watcher = true,
        group_empty_dirs = true,
        window = {
          mappings = {
            ["o"] = "system_open",
            ["e"] = "edit",
            ["i"] = "run_command",
          },
        },
        commands = {
          edit = function(state)
            local node = state.tree:get_node()
            local path = node:get_id()
            vim.api.nvim_command("e " .. path)
          end,
          run_command = function(state)
            local node = state.tree:get_node()
            local path = node:get_id()
            vim.api.nvim_input(": " .. path .. "<Home>")
          end,
          system_open = function(state)
            local node = state.tree:get_node()
            local path = node:get_id()
            vim.api.nvim_command("silent !xdg-open '" .. path .. "'")
          end,
        },
      },
    },
  },
  {
    "elihunter173/dirbuf.nvim",
    cmd = "Dirbuf",
  },
}
