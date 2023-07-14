local function debug_continue()
  local dap = require("dap")
  if not dap.session() then
    require('dap.ext.vscode').load_launchjs(nil, { codelldb = { 'c', 'cpp' }, cppdbg = { 'c', 'cpp' } })
  end

  dap.continue()
end

return {
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      {
        'Joakker/lua-json5',
        build = 'cargo build --release && mv target/release/liblua_json5.so lua/json5.so && cargo clean',
      },
      -- fancy UI for the debugger
      {
        "nvim-telescope/telescope-dap.nvim",
        keys = {
          {
            "<leader>dB",
            function() require('telescope').extensions.dap.list_breakpoints({}) end,
            desc =
            "Breakpoints"
          },
          { "<leader>dF", function() require('telescope').extensions.dap.frames({}) end, desc = "Frames" },
        },
        config = function(_, opts)
          require('telescope').load_extension('dap')
        end
      },
      {
        "rcarriga/nvim-dap-ui",
        -- stylua: ignore
        keys = {
          { "<leader>du", function() require("dapui").toggle({}) end, desc = "Dap UI" },
          { "<C-k>",      function() require("dapui").eval() end,     desc = "Eval",  mode = { "n", "v" } },
          {
            "<leader>dV",
            function()
              require("dapui").float_element("scopes", { enter = true })
            end,
            desc = "Variables"
          },
        },
        opts = {
          layouts = { {
            elements = {
              { id = "repl",    size = 0.5 },
              { id = "console", size = 0.5 }
            },
            position = "bottom",
            size = 10
          }, },
        },
        config = function(_, opts)
          local dap = require("dap")
          local dapui = require("dapui")
          dapui.setup(opts)
          dap.listeners.after.event_initialized["dapui_config"] = function()
            dapui.open({})
          end
          dap.listeners.before.event_terminated["dapui_config"] = function()
            dapui.close({})
          end
          dap.listeners.before.event_exited["dapui_config"] = function()
            dapui.close({})
          end
        end,
      },

      -- which key integration
      {
        "folke/which-key.nvim",
        optional = true,
        opts = {
          defaults = {
            ["<leader>d"] = { name = "+debug" },
          },
        },
      },

      -- mason.nvim integration
      {
        "jay-babu/mason-nvim-dap.nvim",
        dependencies = "mason.nvim",
        cmd = { "DapInstall", "DapUninstall" },
        opts = {
          automatic_installation = true,
          handlers = {},
        },
      },
    },

    -- stylua: ignore
    keys = {
      {
        "<F5>",
        debug_continue,
        desc = "Debug (continue)",
        mode = { "n", "t", "i" },
      },
      {
        "<F6>",
        function() require("dap").step_over() end,
        desc = "Next",
        mode = { "n", "t", "i" },
      },
      {
        "<F7>",
        function() require("dap").step_into() end,
        desc = "Step into",
        mode = { "n", "t", "i" },
      },
      {
        "<F8>",
        function() require("dap").step_out() end,
        desc = "Step out",
        mode = { "n", "t", "i" },
      },
      {
        "<leader>dC",
        function() require("dap").set_breakpoint(vim.fn.input('Breakpoint condition: ')) end,
        desc =
        "Breakpoint Condition"
      },
      {
        "<leader>db",
        function() require("dap").toggle_breakpoint() end,
        desc =
        "Toggle Breakpoint"
      },
      {
        "<leader>dO",
        function()
          local launch_json_path = vim.fn.getcwd() .. '/.vscode/launch.json'
          vim.cmd('e ' .. launch_json_path)
        end,
        desc = "Edit launch.json"
      },
      {
        "<leader>dd",
        debug_continue,
        desc =
        "Debug (continue)"
      },
      {
        "<leader>dc",
        function() require("dap").run_to_cursor() end,
        desc =
        "Run to Cursor"
      },
      {
        "<leader>df",
        function() require("dap").focus_frame() end,
        desc =
        "Frame"
      },
      { "<leader>dj", function() require("dap").down() end, desc = "Down" },
      { "<leader>dk", function() require("dap").up() end,   desc = "Up" },
      {
        "<leader>dl",
        function() require("dap").run_last() end,
        desc =
        "Run Last"
      },
      {
        "<leader>dR",
        function() require("dap").repl.toggle() end,
        desc =
        "Toggle REPL"
      },
      {
        "<leader>dr",
        function() require("dap").restart() end,
        desc =
        "Restart"
      },
      {
        "<leader>dt",
        function() require("dap").terminate() end,
        desc =
        "Terminate"
      },
    },

    config = function()
      require('dap.ext.vscode').json_decode = require('json5').parse
      vim.api.nvim_set_hl(0, "DapStoppedLine", { reverse = true, ctermbg = "darkblue", bg = "darkblue" })
      vim.fn.sign_define("DapBreakpoint", { text = "●", texthl = "DiagnosticError", linehl = "", numhl = "" })
      vim.fn.sign_define(
        "DapBreakpointCondition",
        { text = "⦿", texthl = "DiagnosticError", linehl = "", numhl = "" }
      )
    end,
  }
}
