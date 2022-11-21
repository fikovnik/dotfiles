M = {}

function M.osc52_copy(lines, _)
  local text = table.concat(lines, '\n')
  local data = vim.fn.system([[base64]], text)
  io.stdout:write('\027]52;c;' .. data .. '\a')
  vim.api.nvim_echo({ { string.format('[osc52] %d characters copied', #text), 'Normal' } }, false, {})
end

function M.osc52_paste()
  return { vim.fn.split(vim.fn.getreg(''), '\n'), vim.fn.getregtype('') }
end

function M.mkdir_for_current_file()
  local dir = vim.fn.expand('<afile>:p:h')

  if dir:find('%l+://') == 1 then
    return
  end

  if vim.fn.isdirectory(dir) == 0 then
    vim.fn.mkdir(dir, 'p')
  end
end

return M
