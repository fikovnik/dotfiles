return {
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "kevinhwang91/nvim-ufo",
      dependencies = {
        "kevinhwang91/promise-async",
      },
      config = function()
        vim.o.foldcolumn = "0"
        vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
        vim.o.foldlevelstart = 99
        vim.o.foldenable = true

        local ufo = require("ufo")
        ufo.setup({
          ---@diagnostic disable-next-line: unused-local
          provider_selector = function(bufnr, filetype, buftype)
            -- Use treesitter if available
            if pcall(require, "nvim-treesitter.parsers") then
              if require("nvim-treesitter.parsers").has_parser(filetype) then
                return { "treesitter", "indent" }
              end
            end

            -- Otherwise, might need to disable 'fold providers'
            -- so that we can fallback to the default vim's folding behavior (per foldmethod).
            -- This can be also helpful for a bug where all open/closed folds are lost and reset
            -- whenever fold is updated when, for instance, saving the buffer (when foldlevel != 99).
            -- For more details, see kevinhwang91/nvim-ufo#30
            return ""
          end,
        })

        -- map keys
        local map = vim.keymap.set
        map("n", "zR", ufo.openAllFolds, { desc = "Open all folds" })
        map("n", "zM", ufo.closeAllFolds, { desc = "Close all folds" })
        map("n", "zr", ufo.openFoldsExceptKinds, { desc = "Open fold" })
        map("n", "zm", ufo.closeFoldsWith, { desc = "Close fold" })
      end,
    },
  },
}
