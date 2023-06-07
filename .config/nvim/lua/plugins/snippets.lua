return {
  {
    "dcampos/nvim-snippy",
    event = "VeryLazy",
    dependencies = {
      "honza/vim-snippets",
    },
    config = function(_, opts)
      require("snippy").setup(opts)
      vim.keymap.set("x", "<Tab>", "<Plug>(snippy-cut-text)")
    end,
  },
}
