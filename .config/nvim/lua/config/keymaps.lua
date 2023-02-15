-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local map = vim.keymap.set

local function cmd(c)
  return "<cmd>" .. c .. "<CR>"
end

map("n", "<localleader>e", vim.diagnostic.open_float, { desc = "Errors" })

map("n", "Y", "Vy")

-- copy and paste
map({ "n", "x" }, "<M-w>", '"+y', { silent = true })
map("n", "<M-w><M-w>", '"+yy', { silent = true })

-- better up/down
map("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

-- folds
map("n", "zR", require("ufo").openAllFolds, { desc = "Open all folds" })
map("n", "zM", require("ufo").closeAllFolds, { desc = "Close all folds" })
map("n", "zr", require("ufo").openFoldsExceptKinds, { desc = "Open fold" })
map("n", "zm", require("ufo").closeFoldsWith, { desc = "Close fold" })
map("n", "K", function()
  local winid = require("ufo").peekFoldedLinesUnderCursor()
  if not winid then
    vim.lsp.buf.hover()
  end
end, { remap = true, desc = "Hover" })

-- window movement
map("n", "<M-h>", require("tmux").move_left, { silent = true })
map("n", "<M-j>", require("tmux").move_bottom, { silent = true })
map("n", "<M-k>", require("tmux").move_top, { silent = true })
map("n", "<M-l>", require("tmux").move_right, { silent = true })
map("n", "<M-S-h>", require("tmux").resize_left, { silent = true })
map("n", "<M-S-j>", require("tmux").resize_bottom, { silent = true })
map("n", "<M-S-k>", require("tmux").resize_top, { silent = true })
map("n", "<M-S-l>", require("tmux").resize_right, { silent = true })

-- TODO: Move Lines (use a plugin)
-- map("n", "<M-Down>", "<cmd>m .+1<CR>==", { desc = "Move down" })
-- map("n", "<M-Up>", "<cmd>m .-2<CR>==", { desc = "Move up" })
-- map("v", "<M-Down>", ":m '>+1<CR>gv=gv", { desc = "Move down" })

-- map("v", "<M-Up>", ":m '<-2<CR>gv=gv", { desc = "Move up" })
map("n", "[b", cmd("bprevious"), { desc = "Prev buffer" })
map("n", "]b", cmd("bnext"), { desc = "Next buffer" })
map("n", "[f", cmd("previous"), { desc = "Previous file" })
map("n", "]f", cmd("next"), { desc = "Next file" })
map("n", "[l", cmd("lprevious"), { desc = "Previous loclist" })
map("n", "]l", cmd("lnext"), { desc = "Next loclist" })
map("n", "[q", cmd("cprevious"), { desc = "Previous quickfix" })
map("n", "]q", cmd("cnext"), { desc = "Next quickfix" })
map("n", "[o", cmd("put!=repeat([''],v:count)<bar>']+1"), { desc = "Open below", silent = true })
map("n", "]o", cmd("put =repeat([''],v:count)<bar>'[-1"), { desc = "Open above", silent = true })
map("n", "[t", cmd("tabprevious"), { desc = "Previous tab" })
map("n", "]t", cmd("tabnext"), { desc = "Next tab" })

-- move around
map({ "n", "v" }, "s", cmd("Pounce"))
map("o", "z", cmd("Pounce"))
map("n", "S", cmd("PounceRepeat"))

-- Make `q:` do nothing instead of opening command-line-window, because it is
-- often hit by accident
map("n", "q:", "<Nop>")

-- delete selection in Select mode (helpful when editing snippet placeholders)
map("s", [[<BS>]], [[<BS>i]])

-- other window
map("n", "<C-w><C-w>", "<C-w>w", { silent = true })

-- spelling suggestions
map("n", "z=", cmd("Telescope spell_suggest"), { silent = true })

-- exit term insert mode
map("t", "jk", "<C-\\><C-n>", { silent = true })
map("t", "<ESC><ESC>", "<C-\\><C-n>", { desc = "Enter Normal Mode" })

-- Clear search with <esc>
map({ "i", "n" }, "<esc>", "<cmd>noh<CR><ESC>", { desc = "Escape and clear hlsearch" })

-- https://github.com/mhinz/vim-galore#saner-behavior-of-n-and-n
map("n", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next search result" })
map("x", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next search result" })
map("o", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next search result" })
map("n", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev search result" })
map("x", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev search result" })
map("o", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev search result" })

-- Add undo break-points
map("i", ",", ",<C-g>u")
map("i", ".", ".<C-g>u")
map("i", ";", ";<C-g>u")

-- save file
map({ "i", "v", "n", "s" }, "<C-s>", "<cmd>w<CR><ESC>", { desc = "Save file" })

-- better indenting
map("v", "<", "<gv")
map("v", ">", ">gv")

-- file
map("n", "<leader>fn", cmd("enew"), { desc = "New File" })

--open
map("n", "<leader>ol", cmd("lopen"), { desc = "Location List" })
map("n", "<leader>oq", cmd("copen"), { desc = "Quickfix List" })

-- toggle options
-- map("n", "<leader>uf", require("lazyvim.plugins.lsp.format").toggle, { desc = "Toggle format on Save" })
-- map("n", "<leader>ts", function() Util.toggle("spell") end, { desc = "Toggle Spelling" })
-- map("n", "<leader>tw", function() Util.toggle("wrap") end, { desc = "Toggle Word Wrap" })
-- map("n", "<leader>td", Util.toggle_diagnostics, { desc = "Toggle Diagnostics" })
-- local conceallevel = vim.o.conceallevel > 0 and vim.o.conceallevel or 3
-- map("n", "<leader>tc", function() Util.toggle("conceallevel", false, {0, conceallevel}) end, { desc = "Toggle Conceal" })

-- windows
map("n", "<leader>ww", "<C-W>p", { desc = "Other window" })
map("n", "<leader>wd", "<C-W>c", { desc = "Delete window" })
map("n", "<leader>w-", "<C-W>s", { desc = "Split window below" })
map("n", "<leader>w|", "<C-W>v", { desc = "Split window right" })
