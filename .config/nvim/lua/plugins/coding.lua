return {
  {
    import = "lazyvim.plugins.extras.coding.copilot",
    config = function(_, _)
      vim.cmd("Copilot disable")
    end,
    lazy = false,
  },

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

  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    opts = {
      config = {
        cpp = "// %s",
      },
    },
  },

  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "dcampos/cmp-snippy",
      "kdheepak/cmp-latex-symbols",
      "dcampos/nvim-snippy",
    },
    opts = function(_, opts)
      local cmp = require("cmp")
      local snippy = require("snippy")

      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end

      opts.snippet = {
        expand = function(args)
          snippy.expand_snippet(args.body)
        end,
      }

      opts.preselect = cmp.PreselectMode.None
      opts.completion = { completeopt = "menu,menuone,noselect" }

      opts.mapping = vim.tbl_extend("force", opts.mapping, {
        ["<CR>"] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif snippy.can_expand_or_advance() then
            snippy.expand_or_advance()
          elseif has_words_before() then
            cmp.complete()
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
      })

      opts.sources = cmp.config.sources(vim.list_extend(opts.sources, {
        { name = "snippy" },
        {
          name = "latex_symbols",
          option = {
            strategy = 0, -- mixed
          },
        },
      }))
    end,
  },
}
