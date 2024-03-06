return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "scala" })
      end
    end,
  },

  {
    "scalameta/nvim-metals",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "mfussenegger/nvim-dap",
      "j-hui/fidget.nvim",
    },
    config = function()
      -- Autocmd that will actually be in charging of starting the whole thing
      local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })

      local metals_config = require("metals").bare_config()

      -- Example of settings
      metals_config.settings = {
        showImplicitArguments = true,
        excludedPackages = { "akka.actor.typed.javadsl", "com.github.swagger.akka.javadsl" },
      }

      metals_config.init_options.statusBarProvider = "on"
      metals_config.capabilities = require("cmp_nvim_lsp").default_capabilities()

      metals_config.on_attach = function(_client, _bufnr)
        require("metals").setup_dap()
      end

      vim.api.nvim_create_autocmd("FileType", {
        -- NOTE: You may or may not want java included here. You will need it if you
        -- want basic Java support but it may also conflict if you are using
        -- something like nvim-jdtls which also works on a java filetype autocmd.
        pattern = { "scala", "sbt" },
        callback = function()
          require("metals").initialize_or_attach(metals_config)
        end,
        group = nvim_metals_group,
      })

      local function metals_status_handler(err, status, ctx)
        local val = {}
        -- trim and remove spinner
        local text = status.text:gsub("[⠇⠋⠙⠸⠴⠦]", ""):gsub("^%s*(.-)%s*$", "%1")
        if status.hide then
          val = { kind = "end" }
        elseif status.show then
          val = { kind = "begin", title = text }
        elseif status.text then
          val = { kind = "report", message = text }
        else
          return
        end
        local msg = { token = "metals", value = val }
        vim.lsp.handlers["$/progress"](err, msg, ctx)
      end

      metals_config.init_options.statusBarProvider = "on"
      metals_config.handlers = { ["metals/status"] = metals_status_handler }
    end,
  },

  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "scalameta/nvim-metals",
    },
    opts = {
      setup = {
        metals = {},
      },
    },
  },
}
