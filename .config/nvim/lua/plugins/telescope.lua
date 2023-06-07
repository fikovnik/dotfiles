local Util = require("util")

return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      config = function()
        require("telescope").load_extension("fzf")
      end,
    },
    cmd = "Telescope",
    version = false, -- telescope did only one release, so use HEAD for now
    keys = {
      { "<leader>,", Util.cmd("Telescope buffers show_all_buffers=true"), desc = "Switch Buffer" },
      { "<leader>/", Util.telescope("live_grep"), desc = "Find in Files (Grep)" },
      { "<leader>:", Util.cmd("Telescope command_history"), desc = "Command History" },
      { "<leader><space>", Util.telescope("files"), desc = "Find Files (root dir)" },
      { "<leader>'", Util.cmd("Telescope resume"), desc = "Resume" },
      -- find
      { "<leader>bb", Util.cmd("Telescope buffers"), desc = "Buffers" },
      { "<leader>ff", Util.telescope("files"), desc = "Find Files (root dir)" },
      { "<leader>fF", Util.telescope("files", { cwd = false }), desc = "Find Files (cwd)" },
      { "<leader>fr", Util.cmd("Telescope oldfiles"), desc = "Recent" },
      -- git
      { "<leader>gc", Util.cmd("Telescope git_commits"), desc = "commits" },
      { "<leader>gs", Util.cmd("Telescope git_status"), desc = "status" },
      -- search
      { "<leader>sb", Util.cmd("Telescope current_buffer_fuzzy_find"), desc = "Buffer" },
      { "<leader>sc", Util.cmd("Telescope command_history"), desc = "Command History" },
      { "<leader>sC", Util.cmd("Telescope commands"), desc = "Commands" },
      { "<leader>sd", Util.cmd("Telescope diagnostics"), desc = "Diagnostics" },
      { "<leader>sg", Util.telescope("live_grep"), desc = "Grep (root dir)" },
      { "<leader>sG", Util.telescope("live_grep", { cwd = false }), desc = "Grep (cwd)" },
      { "<leader>sh", Util.cmd("Telescope help_tags"), desc = "Help Pages" },
      { "<leader>sH", Util.cmd("Telescope highlights"), desc = "Search Highlight Groups" },
      { "<leader>sk", Util.cmd("Telescope keymaps"), desc = "Key Maps" },
      { "<leader>sM", Util.cmd("Telescope man_pages"), desc = "Man Pages" },
      { "<leader>sm", Util.cmd("Telescope marks"), desc = "Jump to Mark" },
      { "<leader>so", Util.cmd("Telescope vim_options"), desc = "Options" },
      { "<leader>sw", Util.telescope("grep_string"), desc = "Word (root dir)" },
      { "<leader>sW", Util.telescope("grep_string", { cwd = false }), desc = "Word (cwd)" },
      -- toggle
      {
        "<leader>tC",
        Util.telescope("colorscheme", { enable_preview = true }),
        desc = "Colorscheme with preview",
      },
    },
    opts = {
      defaults = {
        prompt_prefix = " ",
        selection_caret = " ",
        mappings = {
          i = {
            ["<C-h>"] = "which_key",
            ["<C-g>"] = "close",
            ["<c-t>"] = function(...)
              return require("trouble.providers.telescope").open_with_trouble(...)
            end,
            ["<a-i>"] = function()
              Util.telescope("find_files", { no_ignore = true })()
            end,
            ["<a-h>"] = function()
              Util.telescope("find_files", { hidden = true })()
            end,
            ["<C-Down>"] = "cycle_history_next",
            ["<C-Up>"] = "cycle_history_prev",
            ["<C-f>"] = "preview_scrolling_down",
            ["<C-b>"] = "preview_scrolling_up",
          },
          n = {
            ["q"] = "close",
          },
        },
      },
      pickers = {
        buffers = {
          mappings = {
            i = {
              ["<C-d>"] = "delete_buffer",
            },
            n = {
              ["<C-d>"] = "delete_buffer",
            },
          },
        },
      },
    },
  },
}
