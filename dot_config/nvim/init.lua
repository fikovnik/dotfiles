-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")

-- start vim with copilot disabled by default
vim.cmd("silent! Copilot disable")
