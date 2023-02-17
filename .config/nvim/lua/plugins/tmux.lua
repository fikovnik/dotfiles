return {
  {
    "aserowy/tmux.nvim",
    keys = {
      -- {
      --   "<M-H>",
      --   function()
      --     require("tmux").resize_left()
      --   end,
      --   desc = "Resize left",
      -- },
      -- {
      --   "<M-J>",
      --   function()
      --     require("tmux").resize_bottom()
      --   end,
      --   desc = "Resize down",
      -- },
      -- {
      --   "<M-K>",
      --   function()
      --     require("tmux").resize_top()
      --   end,
      --   desc = "Resize top",
      -- },
      -- {
      --   "<M-L>",
      --   function()
      --     require("tmux").resize_right()
      --   end,
      --   desc = "Resize down",
      -- },
      {
        "<M-h>",
        function()
          require("tmux").move_left()
        end,
        desc = "Move left",
      },
      {
        "<M-j>",
        function()
          require("tmux").move_bottom()
        end,
        desc = "Move down",
      },
      {
        "<M-k>",
        function()
          require("tmux").move_top()
        end,
        desc = "Move up",
      },
      {
        "<M-l>",
        function()
          require("tmux").move_right()
        end,
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
}
