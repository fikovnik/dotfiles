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
    -- stylua: ignore
    keys = {
      { "<leader>,", Util.telescope("buffers", { show_all_buffers = true }), desc = "Switch Buffer" },
      { "<leader>/", Util.telescope("live_grep"), desc = "Find in Files (grep)" },
      { "<leader>*", Util.telescope("grep_string"), desc = "Find in Files (word)" },
      { "<leader>:", Util.telescope("command_history"), desc = "Command History" },
      { "<leader><space>", Util.telescope("files"), desc = "Find Files (root dir)" },
      { "<leader>'", Util.telescope("resume"), desc = "Resume" },
      -- find
      { "<leader>bb", Util.telescope("buffers"), desc = "Buffers" },
      { "<leader>ff", Util.telescope("files", { cwd = false }),     desc = "Find Files (cwd)" },
      { "<leader>fF", Util.telescope("fd", { find_command = { 'fd', '--unrestricted' } }), desc = "Find All Files" },
      { "<leader>fr", Util.telescope("oldfiles"), desc = "Recent" },
      -- search
      { "<leader>sb", Util.telescope("current_buffer_fuzzy_find"), desc = "Buffer" },
      { "<leader>sc", Util.telescope("command_history"), desc = "Command History" },
      { "<leader>sC", Util.telescope("commands"), desc = "Commands" },
      { "<M-x>", Util.telescope("commands"), desc = "Commands" },
      { "<leader>sd", Util.telescope("diagnostics"), desc = "Diagnostics" },
      { "<leader>sg", Util.telescope("live_grep"), desc = "Grep (root dir)" },
      { "<leader>sG", Util.telescope("live_grep", { cwd = false }), desc = "Grep (cwd)" },
      { "<leader>sh", Util.telescope("help_tags"), desc = "Help Pages" },
      { "<leader>sH", Util.telescope("highlights"), desc = "Search Highlight Groups" },
      { "<leader>sk", Util.telescope("keymaps"), desc = "Key Maps" },
      { "<leader>sM", Util.telescope("man_pages"), desc = "Man Pages" },
      { "<leader>sm", Util.telescope("marks"), desc = "Jump to Mark" },
      { "<leader>so", Util.telescope("vim_options"), desc = "Options" },
      { "<leader>sw", Util.telescope("grep_string"), desc = "Word (root dir)" },
      { "<leader>sW", Util.telescope("grep_string", { cwd = false }), desc = "Word (cwd)" },
      -- vim
      { "<leader>vC", Util.telescope("colorscheme", { enable_preview = true }), desc = "Colorscheme with preview", },
      -- spell
      { "z=", Util.telescope("spell_suggest"), desc = "Spell suggest" }
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
