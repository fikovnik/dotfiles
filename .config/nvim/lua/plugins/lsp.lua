local Util = require("util")

local function format_on_save(client, buf)
  if client.supports_method("textDocument/formatting") then
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = vim.api.nvim_create_augroup("LspFormat." .. buf, {}),
      buffer = buf,
      callback = function()
        vim.lsp.buf.format({
          timeout_ms = nil,
          buffer = buf,
        })
      end,
    })
  end
end

---@diagnostic disable-next-line: unused-local
local function set_keymap(client, buf)
  ---@diagnostic disable-next-line: redefined-local
  local function map(mode, lhs, rhs, desc, opts)
    local local_opts = { buffer = buf, silent = true, desc = desc }
    if opts then
      local_opts = vim.tbl_extend("force", opts, local_opts)
    end
    vim.keymap.set(mode, lhs, rhs, local_opts)
  end

  map("n", "K", vim.lsp.buf.hover, "Hover", { remap = false })
  map({ "n", "v" }, "<M-CR>", vim.lsp.buf.code_action, "Actions")

  map("n", "<localleader>D", function()
    vim.lsp.buf.declaration({ reuse_win = true })
  end, "Declaration")

  map("n", "<localleader>d", function()
    vim.lsp.buf.definition({ reuse_win = true })
  end, "Definition")

  map("n", "<localleader>t", function()
    vim.lsp.buf.type_definition({ reuse_win = true })
  end, "Type")

  map("n", "<localleader>i", vim.lsp.buf.implementation, "Implementation")
  map("n", "<localleader>r", vim.lsp.buf.references, "References")
  map("n", "<localleader>R", vim.lsp.buf.rename, "Rename")

  map("n", "<localleader>f", function()
    vim.lsp.buf.format({ async = true })
  end, "Format")

  map("n", "<localleader><localleader>", Util.cmd("Telescope lsp_document_symbols"), "Symbols")
  map("n", "<localleader>l", Util.cmd("Telescope lsp_workspace_symbols"), "All symbols")
  map("n", "<localleader>/", Util.cmd("Telescope lsp_dynamic_workspace_symbols"), "Search symbols")
  map({ "n", "i" }, "<M-p>", vim.lsp.buf.signature_help, "Signature")
  map("n", "<localleader>ci", vim.lsp.buf.incoming_calls, "Incoming calls")
  map("n", "<localleader>co", vim.lsp.buf.outgoing_calls, "Outgoing calls")
  map("v", "<localleader>f", vim.lsp.buf.range_formatting, "Format")
  map("n", "<localleader>wA", vim.lsp.buf.add_workspace_folder, "Add folder")
  map("n", "<localleader>wR", vim.lsp.buf.remove_workspace_folder, "Remove folder")
  map("n", "<localleader>wL", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, "List folders")

  map("n", "]d", Util.diagnostic_goto(true), "Next Diagnostic")
  map("n", "[d", Util.diagnostic_goto(false), "Prev Diagnostic")
  map("n", "]e", Util.diagnostic_goto(true, "ERROR"), "Next Error")
  map("n", "[e", Util.diagnostic_goto(false, "ERROR"), "Prev Error")
  map("n", "]w", Util.diagnostic_goto(true, "WARN"), "Next Warning")
  map("n", "[w", Util.diagnostic_goto(false, "WARN"), "Prev Warning")
  map("n", "<localleader>e", vim.diagnostic.open_float, { desc = "Errors (line)" })
  map("n", "<localleader>ll", vim.lsp.codelens.run, { desc = "Lens" })
  map("n", "<localleader>lr", vim.lsp.codelens.refresh, { desc = "Lens" })

  local wk = require("which-key")
  wk.register({
    ["<localleader>c"] = { name = "+calls" },
    ["<localleader>l"] = { name = "+lens" },
    ["<localleader>w"] = { name = "+workspace" },
  })
end

return {
  {
    "neovim/nvim-lspconfig",
    event = "BufReadPre",
    dependencies = {
      { "folke/neoconf.nvim", cmd = "Neoconf", config = true },
      { "folke/neodev.nvim", opts = { experimental = { pathStrict = true } } },
      "mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp",
    },
    ---@class PluginLspOpts
    opts = {
      -- options for vim.diagnostic.config()
      diagnostics = {
        underline = true,
        update_in_insert = false,
        virtual_text = { spacing = 4, prefix = "●" },
        severity_sort = true,
      },
      -- LSP Server Settings
      ---@type lspconfig.options
      servers = {
        jsonls = {},
        lua_ls = {
          -- mason = false, -- set to false if you don't want this server to be installed with mason
          settings = {
            Lua = {
              workspace = {
                checkThirdParty = false,
              },
              completion = {
                callSnippet = "Replace",
              },
            },
          },
        },
      },
      -- you can do any additional lsp server setup here
      -- return true if you don't want this server to be setup with lspconfig
      ---@type table<string, fun(server:string, opts:_.lspconfig.options):boolean?>
      setup = {
        -- example to setup with typescript.nvim
        -- tsserver = function(_, opts)
        --   require("typescript").setup({ server = opts })
        --   return true
        -- end,
        -- Specify * to use this function as a fallback for any server
        -- ["*"] = function(server, opts) end,
      },
    },
    ---@param opts PluginLspOpts
    config = function(plugin, opts)
      -- setup autoformat
      require("util").on_attach(format_on_save)
      -- setup keybindings
      require("util").on_attach(set_keymap)
      -- diagnostics
      vim.diagnostic.config(opts.diagnostics)

      local servers = opts.servers
      local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

      local function setup(server)
        local server_opts = vim.tbl_deep_extend("force", {
          capabilities = vim.deepcopy(capabilities),
        }, servers[server] or {})

        if opts.setup[server] then
          if opts.setup[server](server, server_opts) then
            return
          end
        elseif opts.setup["*"] then
          if opts.setup["*"](server, server_opts) then
            return
          end
        end
        require("lspconfig")[server].setup(server_opts)
      end

      local mlsp = require("mason-lspconfig")
      local available = mlsp.get_available_servers()

      local ensure_installed = {} ---@type string[]
      for server, server_opts in pairs(servers) do
        if server_opts then
          server_opts = server_opts == true and {} or server_opts
          -- run manual setup if mason=false or if this is a server that cannot be installed with mason-lspconfig
          if server_opts.mason == false or not vim.tbl_contains(available, server) then
            setup(server)
          else
            ensure_installed[#ensure_installed + 1] = server
          end
        end
      end

      require("mason-lspconfig").setup({ ensure_installed = ensure_installed })
      require("mason-lspconfig").setup_handlers({ setup })
    end,
  },

  -- formatters
  {
    "jose-elias-alvarez/null-ls.nvim",
    event = "BufReadPre",
    dependencies = { "mason.nvim" },
    opts = function()
      local nls = require("null-ls")
      return {
        sources = {
          -- nls.builtins.formatting.prettierd,
          nls.builtins.formatting.stylua,
        },
      }
    end,
  },

  -- cmdline tools and lsp servers
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    opts = {
      ensure_installed = {
        "stylua",
        "shellcheck",
        "shfmt",
      },
    },
    config = function(_, opts)
      require("mason").setup(opts)
      local mr = require("mason-registry")
      for _, tool in ipairs(opts.ensure_installed) do
        local p = mr.get_package(tool)
        if not p:is_installed() then
          p:install()
        end
      end
    end,
  },
}
