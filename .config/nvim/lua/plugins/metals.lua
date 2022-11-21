local status_ok, metals = pcall(require, 'metals')
if not status_ok then return end

local function metals_status_handler(err, status, ctx)
  local val = {}
  -- trim and remove spinner
  local text = status.text:gsub('[⠇⠋⠙⠸⠴⠦]', ''):gsub('^%s*(.-)%s*$', '%1')
  if status.hide then
    val = { kind = 'end' }
  elseif status.show then
    val = { kind = 'begin', title = text }
  elseif status.text then
    val = { kind = 'report', message = text }
  else
    return
  end
  local msg = { token = 'metals', value = val }
  vim.lsp.handlers['$/progress'](err, msg, ctx)
end

local metals_config = metals.bare_config()

metals_config.settings = {
  showImplicitArguments = true,
  excludedPackages = { 'akka.actor.typed.javadsl', 'com.github.swagger.akka.javadsl' },
}

metals_config.init_options.statusBarProvider = 'on'
metals_config.capabilities = require('cmp_nvim_lsp').default_capabilities()
metals_config.handlers = { ['metals/status'] = metals_status_handler }

local dap = require('dap')

dap.configurations.scala = {
  {
    type = 'scala',
    request = 'launch',
    name = 'RunOrTest',
    metals = {
      runType = 'runOrTestFile',
      --args = { 'firstArg', 'secondArg', 'thirdArg' }, -- here just as an example
    },
  },
  {
    type = 'scala',
    request = 'launch',
    name = 'Test Target',
    metals = {
      runType = 'testTarget',
    },
  },
}

metals_config.on_attach = function(client, bufnr)
  require('metals').setup_dap()
  require('keybinds').set_lsp_integration(bufnr)
end

-- Autocmd that will actually be in charging of starting the whole thing
local nvim_metals_group = vim.api.nvim_create_augroup('nvim-metals', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  pattern = { 'scala', 'sbt', 'java' },
  callback = function()
    require('metals').initialize_or_attach(metals_config)
  end,
  group = nvim_metals_group,
})
