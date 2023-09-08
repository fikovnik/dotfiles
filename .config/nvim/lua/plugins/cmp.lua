return {
  {
    "hrsh7th/nvim-cmp",
    version = false, -- last release is way too old
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "onsails/lspkind.nvim",
      "dcampos/cmp-snippy",
      "kdheepak/cmp-latex-symbols",
    },
    opts = function()
      local cmp = require("cmp")
      local snippy = require("snippy")

      return {
        preselect = cmp.PreselectMode.None,
        completion = {
          completeopt = "menu,menuone,noinsert,noselect",
        },
        snippet = {
          expand = function(args)
            -- luasnip.lsp_expand(args.body)
            snippy.expand_snippet(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-p>"] = cmp.mapping.select_prev_item(),
          ["<C-n>"] = cmp.mapping.select_next_item(),
          ["<C-u>"] = cmp.mapping.scroll_docs(-4),
          ["<C-d>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-g>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
          -- supertab
          ["<Tab>"] = cmp.mapping(function(fallback)
            if snippy.can_expand_or_advance() then
              snippy.expand_or_advance()
            elseif cmp.visible() then
              cmp.select_next_item()
            else
              fallback()
            end
          end, { "i", "s" }),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif snippy.can_jump(-1) then
              snippy.previous()
            else
              fallback()
            end
          end, { "i", "s" }),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "snippy" },
          { name = "buffer" },
          { name = "path" },
          {
            name = "latex_symbols",
            option = {
              strategy = 0, -- mixed
            },
          },
        }),
        formatting = {
          format = require("lspkind").cmp_format({
            mode = "symbol_text",
            maxwidth = 50,
            ellipsis_char = "...",
          }),
        },
        experimental = {
          ghost_text = {
            hl_group = "LspCodeLens",
          },
        },
        window = {
          documentation = cmp.config.window.bordered(),
        },
      }
    end,
  },
}
