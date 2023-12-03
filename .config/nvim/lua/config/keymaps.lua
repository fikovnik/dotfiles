-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local Util = require("util")
local map = vim.keymap.set

-- Commmand mode
map("c", "<C-BS>", "<C-W>")
map("c", "<C-a>", "<Home>")
map("c", "<C-e>", "<End>")
map("c", "<C-k>", "<C-\\>estrpart(getcmdline(),0,getcmdpos()-1)<CR>")

-- Insert mode
map("i", "<C-e>", "<C-o>$")
map("i", "<C-a>", "<C-o>^")
map("i", "<C-BS>", "<C-W>")
map("i", "<M-f>", "<C-o>w")
map("i", "<M-b>", "<C-o>b")
map("i", "<M-d>", "<C-o>dw")
map("i", "<M-q>", "<C-o>gqap")

-- Edit
map({ "n", "v" }, "<leader>es", ":sort<CR>", { desc = "Sort lines" })
map({ "n", "v" }, "<leader>e<space>", ":StripWhitespace<CR>", { desc = "Strip whitespace" })
map("n", "<leader>ea", ":keepjumps normal! ggVG<cr>", { desc = "Select all" })
map("n", "<M-q>", "gwap", { desc = "Format paragraph" })

vim.cmd([[unmap Y]])

-- copy and paste
map({ "n", "x" }, "<M-w>", '"+y', { silent = true })
map("n", "<M-w><M-w>", '"+yy', { silent = true })

-- better up/down
map("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

map("n", "[b", Util.cmd("bprevious"), { desc = "Prev buffer" })
map("n", "]b", Util.cmd("bnext"), { desc = "Next buffer" })
map("n", "[f", Util.cmd("previous"), { desc = "Previous file" })
map("n", "]f", Util.cmd("next"), { desc = "Next file" })
map("n", "[l", Util.cmd("lprevious"), { desc = "Previous loclist" })
map("n", "]l", Util.cmd("lnext"), { desc = "Next loclist" })
map("n", "[q", Util.cmd("cprevious"), { desc = "Previous quickfix" })
map("n", "]q", Util.cmd("cnext"), { desc = "Next quickfix" })
map("n", "[o", Util.cmd("put!=repeat([''],v:count)<bar>']+1"), { desc = "Open below", silent = true })
map("n", "]o", Util.cmd("put =repeat([''],v:count)<bar>'[-1"), { desc = "Open above", silent = true })
map("n", "[t", Util.cmd("tabprevious"), { desc = "Previous tab" })
map("n", "]t", Util.cmd("tabnext"), { desc = "Next tab" })
map("n", "<M-[>", Util.cmd("tabprevious"), { desc = "Previous tab" })
map("n", "<M-]>", Util.cmd("tabnext"), { desc = "Previous tab" })
map("t", "<C-\\><C-n><M-[>", Util.cmd("tabprevious"), { desc = "Previous tab" })
map("t", "<C-\\><C-n><M-]>", Util.cmd("tabnext"), { desc = "Previous tab" })

-- move around
map({ "n", "x" }, "s", Util.cmd("Pounce"))
map("o", "z", Util.cmd("Pounce"))
map("n", "S", Util.cmd("PounceRepeat"))

-- Make `q:` do nothing instead of opening command-line-window, because it is
-- often hit by accident
map("n", "q:", "<Nop>")

-- delete selection in Select mode (helpful when editing snippet placeholders)
map("s", [[<BS>]], [[<BS>i]])

-- other window
map("n", "<C-w><C-w>", "<C-w>w", { silent = true })

-- exit term insert mode
map("t", "jk", "<C-\\><C-n>", { silent = true })

-- Clear search with <esc>
map({ "i", "n" }, "<esc>", "<cmd>noh<CR><ESC>", { desc = "Escape and clear hlsearch" })

-- https://github.com/mhinz/vim-galore#saner-behavior-of-n-and-n
map("n", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next search result" })
map("x", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next search result" })
map("o", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next search result" })
map("n", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev search result" })
map("x", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev search result" })
map("o", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev search result" })

-- -- Add undo break-points
-- map("i", ",", ",<C-g>u")
-- map("i", ".", ".<C-g>u")
-- map("i", ";", ";<C-g>u")

-- save file
map({ "i", "v", "n", "s" }, "<C-s>", "<cmd>w<CR><ESC>", { desc = "Save file" })

-- better indenting
map("v", "<", "<gv")
map("v", ">", ">gv")

-- fix n and N keeping cursor in center
map("n", "n", "nzz", { silent = true })
map("n", "N", "Nzz", { silent = true })

-- search visually highlighted text
map("v", "<M-/>", [[y/\V<C-R>=escape(@",'/\')<CR><CR>]])

-- delete selection in Select mode (helpful when editing snippet placeholders)
map("s", [[<BS>]], [[<BS>i]])

-- File
map("n", "<leader>fn", Util.cmd("enew"), { desc = "New File" })

--Open
map("n", "<leader>ol", Util.cmd("lopen"), { desc = "Location List" })
map("n", "<leader>oq", Util.cmd("copen"), { desc = "Quickfix List" })

-- Vim
map("n", "<leader>vq", Util.cmd("qa"), { desc = "Quit all" })

-- Toggle
map("n", "<leader>vtw", Util.cmd("set wrap!"), { silent = true, desc = "Wrap" })
map("n", "<leader>vtW", Util.cmd("set list!"), { silent = true, desc = "Whitespaces" })
map("n", "<leader>vts", Util.cmd("set spell!"), { silent = true, desc = "Spell" })
map("n", "<leader>vtf", require("util.format").toggle, { silent = true, desc = "Toggle format on Save" })
-- TODO: conceal
-- TODO: diagnostics
-- TODO: hydra
-- https://github.com/anuvyklack/hydra.nvim/wiki/Vim-Options

-- Windows
map("n", "<leader>ww", "<C-W>p", { desc = "Other window" })
map("n", "<leader>wd", "<C-W>c", { desc = "Delete window" })
map("n", "<leader>w-", "<C-W>s", { desc = "Split window below" })
map("n", "<leader>w|", "<C-W>v", { desc = "Split window right" })
map("n", "<leader>w=", "<C-w>=", { desc = "Balance" })
map("n", "<leader>wH", "<C-w>H", { desc = "Move to left" })
map("n", "<leader>wJ", "<C-w>J", { desc = "Move to bottom" })
map("n", "<leader>wK", "<C-w>K", { desc = "Move to top" })
map("n", "<leader>wL", "<C-w>L", { desc = "Move to right" })
map("n", "<leader>wt", "<cmd>tab split<CR>", { desc = "Open in a new tab" })
-- TODO: maximize

for i = 1, 9 do
  local s = tostring(i)
  map("n", "<C-" .. s .. ">", s .. "<C-w>w", { desc = "Move to window " .. s })
  map("t", "<C-" .. s .. ">", "<C-\\><C-n>" .. s .. "<C-w>w", { desc = "Move to window " .. s })
end

M = {}

return M
