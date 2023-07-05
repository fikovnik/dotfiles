return {
  {
    'stevearc/oil.nvim',
    config = function(_, opts)
      vim.keymap.set("n", "-", require("oil").open, { desc = "Open parent directory" })
      require("oil").setup(opts)
    end,
    opts = {
      columns = {
        "icon",
        "permissions",
        "size",
        "mtime",
      }
    },
    -- Optional dependencies
    dependencies = { "nvim-tree/nvim-web-devicons" },
  }
}
