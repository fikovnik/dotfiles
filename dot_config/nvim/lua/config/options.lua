vim.g.maplocalleader = "\\"
vim.o.mouse = ""
vim.o.relativenumber = false
vim.g.clipboard = {
  name = "OSC 52",
  copy = {
    ["+"] = require("vim.ui.clipboard.osc52").copy("+"),
    ["*"] = require("vim.ui.clipboard.osc52").copy("*"),
  },
  paste = {
    ["+"] = require("vim.ui.clipboard.osc52").paste("+"),
    ["*"] = require("vim.ui.clipboard.osc52").paste("*"),
  },
}
vim.o.clipboard = ""
vim.o.pumblend = 0
vim.o.spelllang = "en,cs"
vim.g.lazyvim_prettier_needs_config = false

vim.g.lazyvim_picker = "snacks"
