return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      textobjects = {
        swap = {
          enable = true,
          swap_next = {
            ["<M->>"] = "@parameter.inner",
          },
          swap_previous = {
            ["<M-<>"] = "@parameter.inner",
          },
        },
      },
    },
  },
}
