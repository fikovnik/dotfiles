-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local Util = require("util")
local map = vim.keymap.set

local function cmd(c)
  return "<cmd>" .. c .. "<CR>"
end

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

-- File
map("n", "<leader>fn", cmd("enew"), { desc = "New File" })

--Open
map("n", "<leader>ol", cmd("lopen"), { desc = "Location List" })
map("n", "<leader>oq", cmd("copen"), { desc = "Quickfix List" })

-- Toggle
map("n", "<leader>tt", cmd("Telescope colorscheme enable_preview=true"), { silent = true, desc = "Themes" })
map("n", "<leader>tw", cmd("set wrap!"), { silent = true, desc = "Wrap" })
map("n", "<leader>tW", cmd("set list!"), { silent = true, desc = "Whitespaces" })
map("n", "<leader>ts", cmd("set spell!"), { silent = true, desc = "Spell" })
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

function M.attach_lsp_keybindings(client, buf)
  ---@diagnostic disable-next-line: redefined-local
  local function map(mode, lhs, rhs, desc, opts)
    local local_opts = { buffer = buf, silent = true, desc = desc }
    if opts then
      local_opts = vim.tbl_extend("force", opts, local_opts)
    end
    vim.keymap.set(mode, lhs, rhs, local_opts)
  end

  map("n", "K", vim.lsp.buf.hover, "Hover", { remap = false })
  map({ "n", "v" }, "<M-CR>", vim.lsp.buf.code_action, "Actions")

  map("n", "<localleader>D", function()
    vim.lsp.buf.declaration({ reuse_win = true })
  end, "Declaration")

  map("n", "<localleader>d", function()
    vim.lsp.buf.definition({ reuse_win = true })
  end, "Definition")

  map("n", "<localleader>t", function()
    vim.lsp.buf.type_definition({ reuse_win = true })
  end, "Type")

  map("n", "<localleader>i", vim.lsp.buf.implementation, "Implementation")
  map("n", "<localleader>r", vim.lsp.buf.references, "References")
  map("n", "<localleader>R", vim.lsp.buf.rename, "Rename")

  map("n", "<localleader>f", function()
    vim.lsp.buf.format({ async = true })
  end, "Format")

  map(
    "n",
    "<localleader><localleader>",
    Util.telescope("lsp_document_symbols", {
      symbols = {
        "Class",
        "Function",
        "Method",
        "Constructor",
        "Interface",
        "Module",
        "Struct",
        "Trait",
        "Field",
        "Property",
      },
    }),
    "Symbols"
  )

  map("n", "<localleader>l", cmd("Telescope lsp_workspace_symbols"), "All symbols")
  map("n", "<localleader>/", cmd("Telescope lsp_dynamic_workspace_symbols"), "Search symbols")
  map({ "n", "i" }, "<M-p>", vim.lsp.buf.signature_help, "Signature")
  map("n", "<localleader>ci", vim.lsp.buf.incoming_calls, "Incoming calls")
  map("n", "<localleader>co", vim.lsp.buf.outgoing_calls, "Outgoing calls")

  map("v", "<localleader>f", vim.lsp.buf.range_formatting, "Format")

  map("n", "<localleader>wA", vim.lsp.buf.add_workspace_folder, "Add folder")
  map("n", "<localleader>wR", vim.lsp.buf.remove_workspace_folder, "Remove folder")
  map("n", "<localleader>wL", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, "List folders")

  map("n", "]d", Util.diagnostic_goto(true), "Next Diagnostic")
  map("n", "[d", Util.diagnostic_goto(false), "Prev Diagnostic")
  map("n", "]e", Util.diagnostic_goto(true, "ERROR"), "Next Error")
  map("n", "[e", Util.diagnostic_goto(false, "ERROR"), "Prev Error")
  map("n", "]w", Util.diagnostic_goto(true, "WARN"), "Next Warning")
  map("n", "[w", Util.diagnostic_goto(false, "WARN"), "Prev Warning")
  map("n", "<localleader>e", vim.diagnostic.open_float, { desc = "Errors (line)" })

  local wk = require("which-key")
  wk.register({
    ["<localleader>c"] = { name = "calls" },
    ["<localleader>w"] = { name = "workspace" },
  })
end

return M
