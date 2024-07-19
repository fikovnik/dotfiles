vim.cmd([[unmap Y]])

-- Disable
vim.keymap.del("n", "<leader>gG") -- disable lazygit binding
vim.keymap.del("n", "<leader>bb") -- disable other buffer binding

-- Local map
vim.keymap.set("n", "<localleader>\\", "<leader>ss", { remap = true, desc = "Goto Symbol" })
vim.keymap.set("n", "<localleader>|", "<leader>sS", { remap = true, desc = "Goto Symbol (Workspace)" })

-- Buffers
vim.keymap.set("n", "<leader>bb", "<leader>,", { remap = true, desc = "Switch Buffer" })

-- Edit
vim.keymap.set({ "n", "v" }, "<leader>es", ":sort<CR>", { desc = "Sort lines" })
vim.keymap.set({ "n", "v" }, "<leader>e<space>", ":StripWhitespace<CR>", { desc = "Strip whitespace" })
vim.keymap.set("n", "<leader>ea", ":keepjumps normal! ggVG<cr>", { desc = "Select all" })
vim.keymap.set("n", "<M-q>", "gwap", { desc = "Format paragraph" })
vim.keymap.set({ "n", "v", "i" }, "<C-e>", "<End>", { desc = "Move to end of line" })
vim.keymap.set({ "n", "v", "i" }, "<C-a>", "<Home>", { desc = "Move to beginning of line" })

-- Copy and paste
vim.keymap.set({ "n", "x" }, "<M-w>", '"+y', { silent = true })
vim.keymap.set("n", "<M-w><M-w>", '"+yy', { silent = true })

-- Toggle
vim.keymap.set("n", "<leader>up", ":Copilot toggle<CR>", { desc = "Toggle Copilot" })
