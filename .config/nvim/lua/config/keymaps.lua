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

map("n", "Y", "Vy")

-- copy and paste
map({ "n", "x" }, "<M-w>", '"+y', { silent = true })
map("n", "<M-w><M-w>", '"+yy', { silent = true })

-- better up/down
map("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

-- window movement
-- TODO: Move Lines (use a plugin)
-- map("n", "<M-Down>", "<cmd>m .+1<CR>==", { desc = "Move down" })
-- map("n", "<M-Up>", "<cmd>m .-2<CR>==", { desc = "Move up" })
-- map("v", "<M-Down>", ":m '>+1<CR>gv=gv", { desc = "Move down" })

-- map("v", "<M-Up>", ":m '<-2<CR>gv=gv", { desc = "Move up" })
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

-- move around
map({ "n", "v" }, "s", Util.cmd("Pounce"))
map("o", "z", Util.cmd("Pounce"))
map("n", "S", Util.cmd("PounceRepeat"))

-- Make `q:` do nothing instead of opening command-line-window, because it is
-- often hit by accident
map("n", "q:", "<Nop>")

-- delete selection in Select mode (helpful when editing snippet placeholders)
map("s", [[<BS>]], [[<BS>i]])

-- other window
map("n", "<C-w><C-w>", "<C-w>w", { silent = true })

-- spelling suggestions
map("n", "z=", Util.cmd("Telescope spell_suggest"), { silent = true })

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

-- File
map("n", "<leader>fn", Util.cmd("enew"), { desc = "New File" })

--Open
map("n", "<leader>ol", Util.cmd("lopen"), { desc = "Location List" })
map("n", "<leader>oq", Util.cmd("copen"), { desc = "Quickfix List" })

-- Toggle
map("n", "<leader>tt", Util.cmd("Telescope colorscheme enable_preview=true"), { silent = true, desc = "Themes" })
map("n", "<leader>tw", Util.cmd("set wrap!"), { silent = true, desc = "Wrap" })
map("n", "<leader>tW", Util.cmd("set list!"), { silent = true, desc = "Whitespaces" })
map("n", "<leader>ts", Util.cmd("set spell!"), { silent = true, desc = "Spell" })
-- TODO: conceal
-- TODO: diagnostics

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

M = {}

return M
