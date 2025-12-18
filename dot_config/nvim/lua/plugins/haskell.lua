return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "haskell" } },
  },
  {
    "mrcjkb/haskell-tools.nvim",
    lazy = false, -- This plugin is already lazy
    ft = { "haskell", "lhaskell", "cabal", "cabalproject" },
    init = function()
      vim.g.haskell_tools = {
        hls = {
          settings = {
            haskell = {
              plugin = {
                semanticTokens = {
                  globalOn = true,
                },
              },
            },
          },
        },
      }
    end,
    keys = {
      {
        "<leader>cE",
        function()
          require("haskell-tools").lsp.buf_eval_all()
        end,
        ft = "haskell",
        desc = "Evaluate All (haskell)",
      },
      {
        "<leader>cH",
        function()
          require("haskell-tools").hoogle.hoogle_signature()
        end,
        ft = "haskell",
        desc = "Hoogle Signature (haskell)",
      },
    },
  },
  -- {
  --   "stevearc/conform.nvim",
  --   opts = {
  --     formatters_by_ft = {
  --       haskell = { "fourmolu" },
  --       cabal = { "cabal_fmt" },
  --     },
  --   },
  -- },
}
