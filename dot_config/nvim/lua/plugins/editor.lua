return {
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
    },
    keys = {
      { "<leader>gg", "<cmd>Neogit<cr>", desc = "Status" },
    },
    cmd = {
      "Neogit",
    },
    opts = {
      disable_hint = true,
      signs = {
        section = { "", "" },
        item = { "", "" },
        hunk = { "", "" },
      },
      integrations = {
        diffview = true,
      },
    },
  },

  {
    "sindrets/diffview.nvim",
    keys = {
      { "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "Diffview" },
      { "<leader>gH", "<cmd>DiffviewFileHistory<cr>", desc = "History" },
    },
    cmd = {
      "DiffviewOpen",
      "DiffviewFileHistory",
    },
    opts = {
      use_icons = false,
      view = {
        merge_tool = {
          layout = "diff3_mixed",
        },
      },
    },
  },

  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin",
    },
  },

  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
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
      },
    },
  },

  {
    "folke/trouble.nvim",
    opts = {
      use_diagnostic_signs = true,
      auto_preview = false,
    },
  },

  {
    "gbprod/yanky.nvim",
    opts = function(_, opts)
      opts.system_clipboard = {
        sync_with_ring = false,
      }
    end,
  },

  {
    "folke/flash.nvim",
    opts = {
      label = {
        current = false,
      },
      modes = {
        search = {
          enabled = false,
        },
      },
    },
  },

  {
    "echasnovski/mini.align",
    version = false,
    config = true,
  },

  {
    "ibhagwan/fzf-lua",
    optional = true,
    opts = function(_, opts)
      local actions = require("fzf-lua.actions")
      local path = require("fzf-lua.path")

      local function open_or_create(selected, ctx, force_create)
        local callopts = (ctx and ctx.__call_opts) or {}
        local picked

        -- try to resolve a real file from the selection
        if not force_create and selected and selected[1] then
          local entry = path.entry_to_file(selected[1], callopts)
          if entry and entry.path and entry.path ~= "" then
            picked = entry.path
          end
        end

        -- if nothing selected, use ctx.query
        if not picked or picked == "" then
          local query = (ctx and ctx.query) or ""
          if query == "" then
            return vim.notify("Nothing selected and query is empty", vim.log.levels.WARN)
          end
          picked = query

          -- compute path = ctx.prompt .. ctx.query
          local base = (ctx and ctx.prompt) or ""
          if not base == "" then
            picked = vim.fs.joinpath(base, picked)
          end
        end

        -- expand ~ and make absolute if needed
        if picked:sub(1, 1) == "~" then
          picked = vim.fn.expand(picked)
        end

        if picked:sub(-1) == "/" then
          vim.fn.mkdir(picked, "p")
          vim.notify("Created directory: " .. picked, vim.log.levels.INFO)
        end
        vim.cmd.edit(vim.fn.fnameescape(picked))
      end

      opts.files = {
        fzf_opts = vim.tbl_extend("force", opts.files.fzf_opts or {}, {
          ["--print-query"] = "",
        }),
        actions = {
          ["ctrl-h"] = actions.toggle_hidden,
          ["ctrl-i"] = actions.toggle_ignore,
          ["default"] = function(selected, ctx)
            return open_or_create(selected, ctx, false)
          end,
          ["ctrl-n"] = function(_, ctx)
            return open_or_create(nil, ctx, true)
          end,
        },
      }
      opts.grep = {
        actions = {
          ["ctrl-h"] = actions.toggle_hidden,
          ["ctrl-i"] = actions.toggle_ignore,
        },
      }

      return opts
    end,
  },
}
