local colors = require('onenord.colors').load()
local lualine_theme = require('lualine.themes.onenord')
for m in pairs(lualine_theme) do
  for k in pairs(lualine_theme[m]) do
    lualine_theme[m][k].bg = colors.highlight_dark
    lualine_theme[m][k].fg = colors.fg
  end
end

lualine_theme.inactive = {
  a = { fg = colors.light_gray, bg = colors.floating },
  b = { fg = colors.light_gray, bg = colors.floating },
  c = { fg = colors.light_gray, bg = colors.active },
}

local function lsp_progress()
  local messages = vim.lsp.util.get_progress_messages()
  if #messages == 0 then
    return ''
  end
  local status = {}
  for _, msg in pairs(messages) do
    table.insert(status, (msg.percentage or 0) .. '%% ' .. (msg.title or ''))
  end
  local spinners = { '⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏' }
  local ms = vim.loop.hrtime() / 1000000
  local frame = math.floor(ms / 120) % #spinners
  return table.concat(status, ' | ') .. ' ' .. spinners[frame + 1]
end

local function lsp_server_name()
    local buf_ft = vim.api.nvim_buf_get_option(0, 'filetype')
    local clients = vim.lsp.get_active_clients()
    if next(clients) == nil then
      return ''
    end
    for _, client in ipairs(clients) do
      local filetypes = client.config.filetypes
      if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
        return '(' .. client.name .. ')'
      end
    end
    return ''
end

require('lualine').setup({
    options = {
        theme = lualine_theme,
        component_separators = '',
        section_separators = '',
        icons_enabled = true,
        globalstatus = true,
    },
    sections = {
        lualine_a = {
            'mode',
            { 'branch', icons_enabled = true, icon = "" },
        },
        lualine_b = {
            { 'filename', file_status = true, path = 1 },
        },
        lualine_c = {
            { 
              'diagnostics',
              sections = { "error", "warn" },
              colored = false,
              always_visible = true,
              symbols = { error = ' ', warn = ' ' }
            },
            { 'diff', colored = false, symbols = { added = "+", modified = "•", removed = "-" } },
--            lsp_progress,
        },
        lualine_x = { 'filetype', lsp_server_name, 'encoding', 'fileformat' },
        lualine_y = { 'progress' },
        lualine_z = { 
          { 
            'location', 
            color = { gui = 'bold' } 
          },
        },
    },
    extensions = { 'quickfix', 'nvim-tree' },
})

-- vim.cmd([[autocmd User LspProgressUpdate let &ro = &ro]])

