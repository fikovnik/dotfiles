return {
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    opts = function()
      return {
        options = {
          theme = "auto",
          globalstatus = true,
          icons_enabled = false,
          component_separators = "",
          section_separators = "",
          disabled_filetypes = { statusline = { "lazy" } },
        },
        sections = {
          lualine_a = {
            {
              "mode",
              fmt = function(str)
                return str:sub(1, 1)
              end,
            },
            { "branch", icons_enabled = true, icon = "" },
          },
          lualine_b = {
            {
              "diagnostics",
              sections = { "error", "warn" },
              -- colored = false,
              -- always_visible = true,
              symbols = { error = " ", warn = " " },
            },
            { "diff", colored = false, symbols = { added = "+", modified = "•", removed = "-" } },
          },
          lualine_c = {
            { "filename", file_status = true, path = 1 },
            -- {
            --   function()
            --     return require("lspsaga.symbolwinbar"):get_winbar()
            --   end,
            --   cond = function()
            --     return package.loaded["lspsaga"] ~= nil and require("lspsaga.symbolwinbar"):get_winbar() ~= nil
            --   end,
            -- },
          },
          lualine_x = { "filetype", require("util").lsp_server_icon },
          lualine_y = {},
          lualine_z = {
            "progress",
            {
              "location",
              color = { gui = "bold" },
            },
          },
        },
        extensions = { "quickfix", "neo-tree", "fugitive", "nvim-dap-ui" },
      }
    end,
  },
}
