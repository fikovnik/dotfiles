return {
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      {
        "theHamsta/nvim-dap-virtual-text",
        opts = { commented = true },
      },
      {
        "rcarriga/nvim-dap-ui",
        opts = {
          icons = {
            collapsed = "",
            current_frame = "",
            expanded = "",
          },
        },
      },
      {
        "nvim-telescope/telescope-dap.nvim",
        config = function()
          require("telescope").load_extension("dap")
        end,
      },
    },
    cmd = { "DapToggleBreakpoint", "DapContinue" },
    -- stylua: ignore
    keys = {
      { "<leader>dC", function() require("dap").set_breakpoint(vim.fn.input "[Condition] > ") end, desc = "Conditional Breakpoint", },
      { "<leader>dD", function() require("dap").disconnect() end,                                  desc = "Disconnect", },
      { "<leader>dE", function() require("dapui").eval(vim.fn.input "[Expression] > ") end,        desc = "Evaluate Input", },
      { "<leader>dF", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.frames)
      end, desc = "Frames", },
      { "<leader>dN", function() require("dap").step_back() end,     desc = "Step Back", },
      { "<leader>dR", function() require("dap").run_to_cursor() end, desc = "Run to Cursor", },
      { "<leader>dS", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.scopes)
      end, desc = "Frames", },
      { "<leader>dU", function() require("dapui").toggle() end,                    desc = "Toggle UI", },
      { "<leader>db", function() require("dap").toggle_breakpoint() end,           desc = "Toggle Breakpoint", },
      { "<leader>dc", function() require("dap").continue() end,                    desc = "Continue", },
      { "<leader>dd", function() require("dap").continue() end,                    desc = "Start", },
      { "<leader>de", function() require("dapui").eval(nil, { enter = true }) end, mode = { "n", "v" },        desc = "Evaluate", },
      { "<leader>dh", function() require("dap.ui.widgets").hover() end,            desc = "Hover Variables", },
      { "<leader>di", function() require("dap").step_into() end,                   desc = "Step Into", },
      { "<leader>di", function() require("dap").run_last() end,                    desc = "Run last", },
      { "<leader>dn", function() require("dap").step_over() end,                   desc = "Step Over", },
      { "<leader>do", function() require("dap").step_out() end,                    desc = "Step Out", },
      { "<leader>dp", function() require("dap").pause.toggle() end,                desc = "Pause", },
      { "<leader>dq", function() require("dap").close() end,                       desc = "Quit", },
      { "<leader>dt", function() require("dap").terminate() end,                   desc = "Terminate", },
      { "<leader>dr", function() require("dap").repl.toggle() end,                 desc = "Toggle REPL", },
      { "<leader>ds", function() require("dap").session() end,                     desc = "Get Session", },
    },
    config = function(plugin, opts)
      local dap = require("dap")
      local dapui = require("dap")

      dap.listeners.after.event_initialized["dapui_config"] = function()
        dap.repl.open()
      end
      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close()
        dap.repl.close()
      end
      dap.listeners.before.event_exited["dapui_config"] = function()
        dapui.close()
        dap.repl.close()
      end

      vim.fn.sign_define("DapBreakpoint", { text = "●", texthl = "DiagnosticError", linehl = "", numhl = "" })
      vim.fn.sign_define("DapBreakpointRejected", { text = "ﰸ", texthl = "DiagnosticError", linehl = "", numhl = "" })
      vim.fn.sign_define(
        "DapBreakpointCondition",
        { text = "⦿", texthl = "DiagnosticError", linehl = "", numhl = "" }
      )
      vim.fn.sign_define("DapStopped", { text = "➔", texthl = "DiagnosticWarn", linehl = "", numhl = "" })
      vim.fn.sign_define("DapLogPoint", { text = "", texthl = "DiagnosticInfo", linehl = "", numhl = "" })

      -- set up debugger
      for k, _ in pairs(opts.setup) do
        opts.setup[k](plugin, opts)
      end
      -- TODO: hydra
      -- https://github.com/anuvyklack/hydra.nvim/issues/3#issuecomment-1162988750
    end,
  },
}
