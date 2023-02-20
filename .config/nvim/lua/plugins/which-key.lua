return {
  {
    "folke/which-key.nvim",
    opts = {
      key_labels = {
        ["<space>"] = "SPC",
        ["<CR>"] = "RET",
        ["<tab>"] = "TAB",
      },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
      wk.register({
        mode = { "n", "v" },
        ["]"] = { name = "+next" },
        ["["] = { name = "+prev" },
        ["<leader>b"] = { name = "+buffer" },
        ["<leader>d"] = { name = "+debug" },
        ["<leader>e"] = { name = "+edit" },
        ["<leader>f"] = { name = "+file" },
        ["<leader>g"] = { name = "+git" },
        ["<leader>gh"] = { name = "+hunks" },
        ["<leader>o"] = { name = "+open" },
        ["<leader>s"] = { name = "+search" },
        ["<leader>t"] = { name = "+test" },
        ["<leader>v"] = { name = "+vim" },
        ["<leader>w"] = { name = "+windows" },
      })
    end,
  },
}
