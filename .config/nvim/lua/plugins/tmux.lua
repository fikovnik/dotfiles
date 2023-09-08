local Util = require("util")

return {
  {
    "aserowy/tmux.nvim",
    keys = {
      {
        "<M-H>",
        function()
          require("tmux").resize_left()
        end,
        mode = { "n", "t" },
        desc = "Resize left",
      },
      {
        "<M-J>",
        function()
          require("tmux").resize_bottom()
        end,
        mode = { "n", "t" },
        desc = "Resize down",
      },
      {
        "<M-K>",
        function()
          require("tmux").resize_top()
        end,
        mode = { "n", "t" },
        desc = "Resize top",
      },
      {
        "<M-L>",
        function()
          require("tmux").resize_right()
        end,
        mode = { "n", "t" },
        desc = "Resize down",
      },
      {
        "<M-h>",
        function()
          require("tmux").move_left()
        end,
        mode = { "n", "t" },
        desc = "Move left",
      },
      {
        "<M-j>",
        function()
          require("tmux").move_bottom()
        end,
        mode = { "n", "t" },
        desc = "Move down",
      },
      {
        "<M-k>",
        function()
          require("tmux").move_top()
        end,
        mode = { "n", "t" },
        desc = "Move up",
      },
      {
        "<M-l>",
        function()
          require("tmux").move_right()
        end,
        mode = { "n", "t" },
        desc = "Move rigth",
      },
    },
    opts = {
      navigation = {
        persist_zoom = true,
        enable_default_keybindings = false,
      },
      resize = {
        enable_default_keybindings = false,
      },
    },
  },
  {
    "jpalardy/vim-slime",
    keys = {
      { mode = "x", "<C-c><C-c>", "<Plug>SlimeRegionSend", desc = "Send region to tmux" },
      { mode = "n", "<C-c><C-c>", "<Plug>SlimeMotionSendib", desc = "Send block to tmux" },
      { mode = "n", "<C-c><C-l>", Util.cmd("SlimeSendCurrentLine"), desc = "Send line to tmux" },
    },
    init = function()
      vim.g.slime_no_mappings = 1
      vim.g.slime_dont_ask_default = 1
    end,
    config = function(_, opts)
      vim.g.slime_target = "tmux"
      vim.g.slime_default_config = {
        socket_name = "default",
        target_pane = "{last}",
      }
    end,
  },
}
