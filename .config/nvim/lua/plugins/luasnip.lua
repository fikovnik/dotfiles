return {
  {
    "L3MON4D3/LuaSnip",
    dependencies = {
      "rafamadriz/friendly-snippets",
      config = function()
        require("luasnip.loaders.from_vscode").lazy_load()
      end,
    },
    opts = {
      history = true,
      delete_check_events = "TextChanged",
    },
    -- stylua: ignore
    keys = {
      { "<leader>eSe", function() require("luasnip.loaders").edit_snippet_files() end, mode = "n", desc = "Edit snippets" },
    },
    config = function()
      require("luasnip.loaders.from_snipmate").lazy_load({ paths = "./snippets" })
    end,
  },
}
