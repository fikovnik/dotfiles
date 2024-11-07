local function debug_continue()
  local dap = require("dap")
  if not dap.session() then
    require("dap.ext.vscode").load_launchjs(nil, { codelldb = { "c", "cpp" }, cppdbg = { "c", "cpp" } })
  end

  dap.continue()
end

local function open_launch_json()
  local launch_json_path = vim.fn.getcwd() .. "/.vscode/launch.json"
  vim.cmd("e " .. launch_json_path)
end

return {
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      {
        "nvim-telescope/telescope-dap.nvim",
        -- stylua: ignore
        keys = {
          { "<leader>dB", function() require("telescope").extensions.dap.list_breakpoints({}) end, desc = "Breakpoints", },
          { "<leader>dF", function() require("telescope").extensions.dap.frames({}) end, desc = "Frames", },
        },
        config = function(_, _)
          require("telescope").load_extension("dap")
        end,
      },
    },
    -- stylua: ignore
    keys = {
      { "<leader>dw", function() require("dap.ui.widgets").hover() end, desc = "Widgets" },
      { "<F5>", debug_continue, desc = "Debug (continue)", mode = { "n", "t", "i" }, },
      { "<F6>", function() require("dap").step_over() end, desc = "Next", mode = { "n", "t", "i" }, },
      { "<F7>", function() require("dap").step_into() end, desc = "Step into", mode = { "n", "t", "i" }, },
      { "<F8>", function() require("dap").step_out() end, desc = "Step out", mode = { "n", "t", "i" }, },
      { "<leader>dO", open_launch_json, desc = "Edit launch.json" },
      { "<leader>dd", debug_continue, desc = "Debug (continue)" },
    },
  },

  {
    "rcarriga/nvim-dap-ui",
        -- stylua: ignore
        keys = {
          { "<leader>dV", function() require("dapui").float_element("scopes", { enter = true }) end, desc = "Variables" },
        },
  },
}
