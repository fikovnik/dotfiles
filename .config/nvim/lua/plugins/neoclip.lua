local status_ok, neoclip = pcall(require, 'neoclip')
if not status_ok then return end

local function is_whitespace(line)
  return vim.fn.match(line, [[^\s*$]]) ~= -1
end

local function all(tbl, check)
  for _, entry in ipairs(tbl) do
    if not check(entry) then
      return false
    end
  end
  return true
end

neoclip.setup {
  keys = {
    telescope = {
      i = {
        select = '<M-CR>',
        paste = '<CR>',
        paste_behind = '<S-CR>',
        replay = '<C-q>',
        delete = '<C-d>',
      },
    },
  },
  filter = function(data)
    return not all(data.event.regcontents, is_whitespace)
  end
}

require('telescope').load_extension('neoclip')
