M = {}

require('keybinds').set_dap_integration()

local dap = require('dap')
dap.adapters.lldb = {
  type = 'server',
  port = '${port}',
  executable = {
    command = 'codelldb',
    args = { '--port', '${port}' },
  },
  sourceLanguages = { 'rust', 'cpp' }
}
dap.adapters.rust = dap.adapters.lldb

-- dap.configurations.rust = {
--   {
--     type = "rust",
--     request = "launch",
--     name = "lldbrust",
--     program = function()
--       local metadata_json = vim.fn.system "cargo metadata --format-version 1 --no-deps"
--       local metadata = vim.fn.json_decode(metadata_json)
--       local target_name = metadata.packages[1].targets[1].name
--       local target_dir = metadata.target_directory
--       return target_dir .. "/debug/" .. target_name
--     end,
--     args = function()
--       local inputstr = vim.fn.input("Params: ", "")
--       local params = {}
--       local sep = "%s"
--       for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
--         table.insert(params, str)
--       end
--       return params
--     end,
--   },
-- }

return M
