vim.g.maplocalleader = "\\"
vim.o.mouse = ""
vim.o.relativenumber = false
vim.o.clipboard = ""
vim.o.pumblend = 0

if os.getenv("SSH_CONNECTION") then
  local function osc52_copy(lines, _)
    local text = table.concat(lines, "\n")
    local data = vim.fn.system([[base64]], text)
    io.stdout:write("\027]52;c;" .. data .. "\a")
    vim.api.nvim_echo({ { string.format("[osc52] %d characters copied", #text), "Normal" } }, false, {})
  end

  local function osc52_paste()
    return { vim.fn.split(vim.fn.getreg(""), "\n"), vim.fn.getregtype("") }
  end

  vim.g.clipboard = {
    name = "osc52",
    copy = { ["+"] = osc52_copy, ["*"] = osc52_copy },
    paste = { ["+"] = osc52_paste, ["*"] = osc52_paste },
  }
end
