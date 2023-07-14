local Util = require("util")

vim.keymap.set("n", "<leader>gG", function()
  Util.float_term({ "lazygit" }, { cwd = Util.get_root() })
end, { desc = "Lazygit (root dir)" })

return {
  {
    "TimUntersberger/neogit",
    keys = {
      { "<leader>gg", Util.cmd("Neogit"), { desc = "Status" } },
    },
    cmd = "Neogit",
    opts = {
      disable_hint = true,
      signs = {
        -- { CLOSED, OPENED }
        section = { "", "" },
        item = { "", "" },
        hunk = { "", "" },
      },
      integrations = {
        diffview = true,
      },
    },
  },
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      on_attach = function(buffer)
        local gs = package.loaded.gitsigns

        local function map(mode, l, r, desc)
          vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
        end

        -- stylua: ignore start
        map("n", "]h", gs.next_hunk, "Next Hunk")
        map("n", "[h", gs.prev_hunk, "Prev Hunk")
        map({ "n", "v" }, "<leader>gr", ":Gitsigns reset_hunk<CR>", "Reset Hunk")
        map("n", "<leader>gp", gs.preview_hunk, "Preview Hunk")
        map("n", "<leader>gb", function() gs.blame_line({ full = true }) end, "Blame Line")
        map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "GitSigns Select Hunk")
      end,
    },
  },
  {
    "sindrets/diffview.nvim",
    keys = {
      { "<leader>gd", Util.cmd("DiffviewOpen"),        desc = "Diff" },
      { "<leader>gh", Util.cmd("DiffviewFileHistory"), desc = "History" },
    },
    cmd = {
      "DiffviewOpen",
      "DiffviewFileHistory",
    },
    opts = {
      use_icons = false,
      view = {
        merge_tool = {
          layout = "diff3_mixed",
        },
      },
    },
  },
}
