vim.cmd([[unmap Y]])

-- Edit
vim.keymap.set("n", "<M-q>", "gwap", { desc = "Format paragraph" })

-- Copy and paste
vim.keymap.set({ "n", "x" }, "<M-w>", '"+y', { silent = true })
vim.keymap.set("n", "<M-w><M-w>", '"+yy', { silent = true })
