return {
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      { "theHamsta/nvim-dap-virtual-text", config = true },
      {
        "rcarriga/nvim-dap-ui",
        opts = {
          icons = {
            collapsed = "",
            current_frame = "",
            expanded = "",
          },
        },
        config = function(_, opts)
          local dapui = require("dapui")

          dapui.setup(opts)

          vim.keymap.set("n", "<Leader>du", dapui.toggle, { desc = "Toggle debugging UI" })
          vim.keymap.set({ "v", "n" }, "<Leader>dK", function()
            dapui.eval(nil, { enter = true })
          end, { desc = "Debug symbol under cursor" })
        end,
      },
    },
    cmd = { "DapToggleBreakpoint" },
    keys = {
      {
        "<Leader>db",
        function()
          require("dap").toggle_breakpoint()
        end,
        desc = "Toggle breakpoint",
      },
      {
        "<Leader>dB",
        function()
          require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: "))
        end,
        desc = "Set breakpoint condition",
      },
      {
        "<Leader>dd",
        function()
          require("dap").continue()
        end,
        desc = "Start/continue",
      },
      {
        "<leader>dc",
        function()
          require("dap").run_to_cursor()
        end,
        desc = "Run to Cursor",
      },
    },
    config = function()
      local dap = require("dap")

      vim.fn.sign_define("DapBreakpoint", { text = "●", texthl = "DiagnosticError", linehl = "", numhl = "" })
      vim.fn.sign_define("DapBreakpointRejected", { text = "ﰸ", texthl = "DiagnosticError", linehl = "", numhl = "" })
      vim.fn.sign_define(
        "DapBreakpointCondition",
        { text = "⦿", texthl = "DiagnosticError", linehl = "", numhl = "" }
      )
      vim.fn.sign_define("DapStopped", { text = "➔", texthl = "DiagnosticWarn", linehl = "", numhl = "" })
      vim.fn.sign_define("DapLogPoint", { text = "", texthl = "DiagnosticInfo", linehl = "", numhl = "" })

      vim.keymap.set("n", "<Leader>dn", dap.step_over, { desc = "Step over" })
      vim.keymap.set("n", "<Leader>di", dap.step_into, { desc = "Step into" })
      vim.keymap.set("n", "<Leader>do", dap.step_out, { desc = "Step out" })
      vim.keymap.set("n", "<Leader>dr", dap.repl.open, { desc = "REPL" })
      vim.keymap.set("n", "<Leader>dR", dap.run_last, { desc = "Run last" })
      vim.keymap.set("n", "<Leader>dq", function()
        dap.terminate({}, {}, function()
          require("dapui").close()
        end)
      end, { desc = "Quit" })

      vim.keymap.set({ "n", "v" }, "<Leader>dh", function()
        require("dap.ui.widgets").hover()
      end, { desc = "Hover" })
      vim.keymap.set({ "n", "v" }, "<Leader>dp", function()
        require("dap.ui.widgets").preview()
      end, { desc = "Preview" })
      vim.keymap.set("n", "<Leader>df", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.frames)
      end, { desc = "frames" })
      vim.keymap.set("n", "<Leader>ds", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.scopes)
      end, { desc = "scopes" })

      -- TODO: hydra
      -- https://github.com/anuvyklack/hydra.nvim/issues/3#issuecomment-1162988750
      -- TODO: telescope
      -- nvim-telescope/telescope-dap.nvim
    end,
  },
}
