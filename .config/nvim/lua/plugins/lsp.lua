local Util = require("util")

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

  local function format()
    require("util.format").format({ force = true })
  end

  map("n", "K", vim.lsp.buf.hover, "Hover", { remap = false })
  map({ "n", "v" }, "<M-CR>", vim.lsp.buf.code_action, "Actions")

  map("n", "<localleader>D", function()
    vim.lsp.buf.declaration({ reuse_win = true })
  end, "Declaration")

  map("n", "<localleader>d", Util.telescope("lsp_definitions"), "Definition")
  map("n", "<localleader>t", Util.telescope("lsp_type_definitions"), "Type")
  map("n", "<localleader>i", Util.telescope("lsp_implementations"), "Implementation")
  map("n", "<localleader>r", Util.telescope("lsp_references"), "References")
  map("n", "<localleader>R", vim.lsp.buf.rename, "Rename")
  map({ "n", "i" }, "<M-p>", vim.lsp.buf.signature_help, "Signature")
  map("n", "<localleader><localleader>", Util.telescope("lsp_document_symbols"), "Symbols")
  map("n", "<localleader>l", Util.telescope("lsp_workspace_symbols"), "All symbols")
  map("n", "<localleader>/", Util.telescope("lsp_dynamic_workspace_symbols"), "Search symbols")
  map("n", "<localleader>ci", Util.telescope("lsp_incoming_calls"), "Incoming calls")
  map("n", "<localleader>co", Util.telescope("lsp_outgoing_calls"), "Outgoing calls")
  map("n", "<localleader>f", format, "Format")
  map("v", "<localleader>f", format, "Format")
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
  map("n", "<localleader>e", vim.diagnostic.open_float, "Errors (line)")
  map("n", "<localleader>E", Util.cmd("TroubleToggle workspace_diagnostics"), "Workspace Diagnostics")
  map("n", "<localleader>ll", vim.lsp.codelens.run, "Run")
  map("n", "<localleader>lr", vim.lsp.codelens.refresh, "Refresh")

  local wk = require("which-key")
  wk.register({
    ["<localleader>c"] = { name = "+calls" },
    ["<localleader>l"] = { name = "+lens" },
    ["<localleader>w"] = { name = "+workspace" },
  })

  local Keys = require("lazy.core.handler.keys")
  local opts = Util.opts("nvim-lspconfig")
  local maps = opts.servers[client.name] and opts.servers[client.name].keys or {}

  for _, keys in ipairs(maps) do
    local o = Keys.opts(keys)
    o.silent = true
    o.buffer = buf
    vim.keymap.set(keys.mode or "n", keys[1], keys[2], o)
  end
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
      autoformat = true,
    },
    config = function(_, opts)
      -- setup autoformat
      require("util.format").setup(opts)
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

      for server, server_opts in pairs(servers) do
        if server_opts then
          server_opts = server_opts == true and {} or server_opts
          -- run manual setup if mason=false or if this is a server that cannot be installed with mason-lspconfig
          if server_opts.mason == false or not vim.tbl_contains(available, server) then
            setup(server)
          end
        end
      end

      require("mason-lspconfig").setup_handlers({ setup })
    end,
  },

  {
    "jose-elias-alvarez/null-ls.nvim",
    event = "BufReadPre",
    dependencies = { "mason.nvim" },
    opts = function()
      local nls = require("null-ls")
      return {
        sources = {
          nls.builtins.formatting.stylua,
          nls.builtins.formatting.shfmt,
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
  {
    "simrat39/symbols-outline.nvim",
    cmd = "SymbolsOutline",
    keys = { { "<localleader>o", Util.cmd("SymbolsOutline"), desc = "Symbols Outline" } },
    opts = {},
  },
  {
    "utilyre/barbecue.nvim",
    name = "barbecue",
    version = "*",
    dependencies = {
      "SmiteshP/nvim-navic",
    },
    opts = {
      show_dirname = false,
      show_basename = false,
      kinds = false,
    },
  },
}
