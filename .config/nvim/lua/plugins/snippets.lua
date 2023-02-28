return {
  {
    "dcampos/nvim-snippy",
    dependencies = {
      "honza/vim-snippets",
    },
    config = function(_, opts)
      require("snippy").setup(opts)
      vim.keymap.set("x", "<Tab>", "<Plug>(snippy-cut-text)")
    end,
  },
}
