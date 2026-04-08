return {
  {
    "jmbuhr/otter.nvim",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {},
  },

  {
    "quarto-dev/quarto-nvim",
    ft = { "quarto" },
    dependencies = { "jmbuhr/otter.nvim", "nvim-treesitter/nvim-treesitter", "jpalardy/vim-slime" },
    opts = {
      lspFeatures = {
        enabled = true,
        languages = { "r", "python", "julia", "bash" },
        diagnostics = { enabled = true, triggers = { "BufWritePost" } },
        completion = { enabled = true },
      },
      codeRunner = {
        enabled = true,
        default_method = "slime",
      },
    },
    keys = {
      {
        "<C-c><C-c>",
        function()
          require("quarto.runner").run_cell()
        end,
        ft = "quarto",
        desc = "Run chunk",
      },
      {
        "<C-c><C-c>",
        function()
          require("quarto.runner").run_range()
        end,
        ft = "quarto",
        mode = "v",
        desc = "Run selection",
      },
      {
        "<C-c><C-a>",
        function()
          require("quarto.runner").run_above()
        end,
        ft = "quarto",
        desc = "Run chunks above",
      },
      {
        "<C-c><C-r>",
        function()
          require("quarto.runner").run_all()
        end,
        ft = "quarto",
        desc = "Run all chunks",
      },
      {
        "<leader>qp",
        function()
          require("quarto").quartoPreview({})
        end,
        ft = "quarto",
        desc = "Quarto preview",
      },
      {
        "<leader>qq",
        function()
          require("quarto").quartoClosePreview()
        end,
        ft = "quarto",
        desc = "Quarto close preview",
      },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, {
        "r",
        "markdown",
        "markdown_inline",
        "yaml",
      })
    end,
  },
}
