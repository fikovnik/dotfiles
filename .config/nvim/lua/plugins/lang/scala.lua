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

return {
  -- add to treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "scala" })
      end
    end,
  },
  -- scala lsp
  {
    "scalameta/nvim-metals",
    dependencies = { "nvim-lua/plenary.nvim" },
    ft = { "scala", "sbt", "java" },
    -- stylua: ignore
    keys = {
      { '<leader>mm', function() require "telescope".extensions.metals.commands() end },
      { '<leader>mc', function() require "metals".compile_cascade() end },
    },
    config = function()
      local metals = require("metals")
      local config = metals.bare_config()

      config.settings = {
        showImplicitArguments = true,
        excludedPackages = { "akka.actor.typed.javadsl", "com.github.swagger.akka.javadsl" },
        serverProperties = { "-Xmx4g" },
        serverVersion = "latest.snapshot",
      }

      config.init_options.statusBarProvider = "on"
      config.handlers = { ["metals/status"] = metals_status_handler }
      config.capabilities = require("cmp_nvim_lsp").default_capabilities()

      config.on_attach = function(client, bufnr)
        metals.setup_dap()
        require("lsp-format").on_attach(client, bufnr)
      end

      -- Autocmd that will actually be in charge of starting the whole thing
      local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "scala", "sbt", "java" },
        callback = function()
          metals.initialize_or_attach(config)
        end,
        group = nvim_metals_group,
      })
    end,
  },
  -- DAP
  {
    "mfussenegger/nvim-dap",
    opts = {
      setup = {
        scala = function(_, opts)
          local dap = require("dap")
          dap.configurations.scala = {
            {
              type = "scala",
              request = "launch",
              name = "RunOrTest",
              metals = {
                runType = "runOrTestFile",
              },
            },
            {
              type = "scala",
              request = "launch",
              name = "Test Target",
              metals = {
                runType = "testTarget",
              },
            },
          }
        end,
      },
    },
  },
}
