vim.cmd([[unmap Y]])

-- Edit
vim.keymap.set({ "n", "v" }, "<leader>es", ":sort<CR>", { desc = "Sort lines" })
vim.keymap.set({ "n", "v" }, "<leader>e<space>", ":StripWhitespace<CR>", { desc = "Strip whitespace" })
vim.keymap.set("n", "<leader>ea", ":keepjumps normal! ggVG<cr>", { desc = "Select all" })
vim.keymap.set("n", "<M-q>", "gwap", { desc = "Format paragraph" })

-- Copy and paste
vim.keymap.set({ "n", "x" }, "<M-w>", '"+y', { silent = true })
vim.keymap.set("n", "<M-w><M-w>", '"+yy', { silent = true })

-- Toggle
vim.keymap.set("n", "<leader>up", ":Copilot toggle<CR>", { desc = "Toggle Copilot" })
