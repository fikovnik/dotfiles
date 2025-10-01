return {
  -- copilot-chat integration with blink
  -- temp fix for: https://github.com/LazyVim/LazyVim/pull/5754
  -- {
  --   "saghen/blink.cmp",
  --   optional = true,
  --   ---@module 'blink.cmp'
  --   ---@type blink.cmp.Config
  --   opts = {
  --     sources = {
  --       providers = {
  --         path = {
  --           -- Path sources triggered by "/" interfere with CopilotChat commands
  --           enabled = function()
  --             return vim.bo.filetype ~= "copilot-chat"
  --           end,
  --         },
  --       },
  --     },
  --   },
  -- },

  -- {
  --   "NickvanDyke/opencode.nvim",
  --   dependencies = {
  --     { "folke/snacks.nvim", opts = { input = { enabled = true } } },
  --   },
  --   config = function()
  --     -- `opencode.nvim` passes options via a global variable instead of `setup()` for faster startup
  --     ---@type opencode.Opts
  --     vim.g.opencode_opts = {
  --       -- Your configuration, if any — see `lua/opencode/config.lua`
  --     }
  --
  --     vim.opt.autoread = true
  --
  --     vim.keymap.set("n", "<leader>ot", function()
  --       require("opencode").toggle()
  --     end, { desc = "Toggle opencode" })
  --     vim.keymap.set("n", "<leader>oA", function()
  --       require("opencode").ask()
  --     end, { desc = "Ask opencode" })
  --     vim.keymap.set("n", "<leader>oa", function()
  --       require("opencode").ask("@cursor: ")
  --     end, { desc = "Ask opencode about this" })
  --     vim.keymap.set("v", "<leader>oa", function()
  --       require("opencode").ask("@selection: ")
  --     end, { desc = "Ask opencode about selection" })
  --     vim.keymap.set("n", "<leader>on", function()
  --       require("opencode").command("session_new")
  --     end, { desc = "New opencode session" })
  --     vim.keymap.set("n", "<leader>oy", function()
  --       require("opencode").command("messages_copy")
  --     end, { desc = "Copy last opencode response" })
  --     vim.keymap.set({ "n", "v" }, "<leader>os", function()
  --       require("opencode").select()
  --     end, { desc = "Select opencode prompt" })
  --   end,
  -- },

  -- {
  --   "olimorris/codecompanion.nvim",
  --   dependencies = {
  --     "nvim-lua/plenary.nvim",
  --     "nvim-treesitter/nvim-treesitter",
  --     "j-hui/fidget.nvim",
  --     {
  --       "MeanderingProgrammer/render-markdown.nvim",
  --       opts = { file_types = { "markdown", "codecompanion" } },
  --     },
  --     {
  --       "echasnovski/mini.diff",
  --       config = function()
  --         local diff = require("mini.diff")
  --         diff.setup({
  --           -- Disabled by default
  --           source = diff.gen_source.none(),
  --         })
  --       end,
  --     },
  --   },
  --   cmd = { "CodeCompanion", "CodeCompanionActions", "CodeCompanionChat", "CodeCompanionCmd" },
  --
  --   opts = {},
  --   keys = {
  --     { "<leader>a", "", desc = "+ai", mode = { "n", "v" } },
  --     { "<leader>aa", "<cmd>CodeCompanionChat Toggle<cr>", mode = { "n", "v" }, desc = "Chat Toggle" },
  --     { "<leader>ax", "<cmd>CodeCompanionActions<cr>", mode = { "n", "v" }, desc = "Actions" },
  --   },
  -- },

  --   -- lua/plugins/avante.lua
  -- return {
  --   -- Completion: Blink (LazyVim already uses this; we just extend config for Avante)
  --   {
  --     "saghen/blink.cmp",
  --     optional = true,
  --     opts = function(_, opts)
  --       opts = opts or {}
  --       opts.compat = opts.compat or {}
  --       -- Enable Avante sources inside Blink (commands, mentions, file targets)
  --       for _, s in ipairs({ "avante_commands", "avante_mentions", "avante_files" }) do
  --         if not vim.tbl_contains(opts.compat, s) then
  --           table.insert(opts.compat, s)
  --         end
  --       end
  --       return opts
  --     end,
  --   },
  -- {
  --   "yetone/avante.nvim",
  --   event = "VeryLazy",
  --   build = "make",
  --   -- Make sure markdown in Avante buffers renders nicely
  --   dependencies = {
  --     "nvim-lua/plenary.nvim",
  --     "MunifTanjim/nui.nvim",
  --
  --     "ibhagwan/fzf-lua", -- file selector "fzf"
  --     "folke/snacks.nvim", -- input "snacks"
  --     "saghen/blink.cmp", -- completion (via compat shim)
  --     "zbirenbaum/copilot.lua", -- for provider = "copilot"
  --     -- {                            -- pretty markdown for Avante buffers
  --     --   "MeanderingProgrammer/render-markdown.nvim",
  --     --   ft = { "markdown", "Avante" },
  --     --   opts = { file_types = { "markdown", "Avante" } },
  --     -- },
  --   },
  --   opts = {
  --     -- -- ===== UI wiring =====
  --     -- file_selector = {
  --     --   -- "native" | "fzf" | "telescope" | "mini.pick"
  --     --   provider = "fzf",          -- use fzf-lua
  --     --   provider_opts = {},        -- (use your global fzf-lua config)
  --     -- },
  --     -- input = {
  --     --   -- "native" | "dressing" | "snacks"
  --     --   provider = "snacks",       -- snacks input UI
  --     --   provider_opts = {
  --     --     title = "Avante",
  --     --     icon = "󰚩 ",
  --     --   },
  --     -- },
  --
  --     -- ===== Providers =====
  --     -- Default to Copilot (Anthropic via Copilot)
  --     provider = "copilot",
  --     providers = {
  --       -- Copilot side (you must be signed in via copilot.lua / GitHub)
  --       copilot = {
  --         -- Pick an Anthropic model exposed by Copilot; Claude Sonnet is a good default
  --         model = "claude-sonnet-4",
  --         -- You can also set temperature / max_tokens here if desired:
  --         -- extra_request_body = { temperature = 0.2, max_tokens = 8192 },
  --       },
  --
  --       -- OpenRouter as a secondary provider (switch with :AvanteSwitchProvider openrouter)
  --       -- Uses OpenAI-compatible wire protocol
  --       openrouter = {
  --         __inherited_from = "openai",
  --         endpoint = "https://openrouter.ai/api/v1",
  --         -- Read the API key from your password store:
  --         --   pass show api/openrouter-nvim
  --         api_key_name = "cmd:pass show api/openrouter-nvim",
  --         -- Your requested default model:
  --         model = "deepseek/deepseek-chat-v3-0324:free",
  --         -- optional: extra_request_body = { temperature = 0.2 },
  --       },
  --     },
  --
  --     -- (optional) Tweak hints / statusline, etc.
  --     -- hints = { enabled = false },
  --   },
  -- },

  {
    "CopilotC-Nvim/CopilotChat.nvim",
    optional = true, -- only loads if the LazyVim extra is enabled
    opts = function(_, opts)
      opts = opts or {}
      opts.providers = opts.providers or {}

      -- Try to get the key via `pass`
      local has_pass = (vim.fn.executable("pass") == 1)
      local api_key

      if has_pass then
        local out = vim.fn.system("pass show api/openrouter-nvim")
        if vim.v.shell_error == 0 then
          api_key = (vim.split(out or "", "\n")[1] or ""):gsub("%s+$", "")
          if api_key == "" then
            api_key = nil
          end
        end
      end

      local enabled = has_pass and (api_key ~= nil)
      if not enabled then
        vim.schedule(function()
          vim.notify(
            "[CopilotChat] openrouter provider disabled (missing `pass` or secret `api/openrouter-nvim`).",
            vim.log.levels.WARN
          )
        end)
        return opts
      end

      -- Register the OpenRouter provider
      opts.providers.openrouter = {
        prepare_input = require("CopilotChat.config.providers").copilot.prepare_input,
        prepare_output = require("CopilotChat.config.providers").copilot.prepare_output,

        get_headers = function()
          return {
            Authorization = "Bearer " .. api_key,
            ["Content-Type"] = "application/json",
          }
        end,

        -- get_models = function(headers)
        --   local response, err = require("CopilotChat.utils").curl_get(
        --     "https://openrouter.ai/api/v1/models",
        --     { headers = headers, json_response = true }
        --   )
        --   if err then
        --     error(err)
        --   end
        --   return vim
        --     .iter(response.body.data)
        --     :map(function(model)
        --       return { id = model.id, name = model.name }
        --     end)
        --     :totable()
        -- end,

        get_models = function(headers)
          local response, err = require("CopilotChat.utils").curl_get(
            "https://openrouter.ai/api/v1/models",
            { headers = headers, json_response = true }
          )
          if err then
            error(err)
          end

          local models = response.body.data or {}

          local result = {}

          for _, model in ipairs(models) do
            if model.id:find(":free$") then
              table.insert(result, { id = model.id, name = model.name })
            end
          end

          return result
        end,

        get_url = function()
          return "https://openrouter.ai/api/v1/chat/completions"
        end,
      }

      opts.model = "gpt-5"
      -- print(vim.inspect(opts))

      return opts
    end,
  },
}
