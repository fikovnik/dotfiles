local map = vim.keymap.set

M = {}

vim.cmd [[unmap Y]]

-- Local mode
map('n', '<localleader>e', vim.diagnostic.open_float, { desc = 'Errors' })
map('n', '<localleader><C-E>', vim.diagnostic.setloclist, { desc = 'Errors to loclist' })

-- Commmand mode
map('c', '<C-BS>', '<C-W>')
map('c', '<C-a>', '<Home>')
map('c', '<C-e>', '<End>')
map('c', '<C-k>', '<C-\\>estrpart(getcmdline(),0,getcmdpos()-1)<CR>')

-- Insert mode
map('i', '<C-e>', '<C-o>$', { silent = true })
map('i', '<C-a>', '<C-o>^', { silent = true })
map('i', '<C-BS>', '<C-W>', { silent = true })
map('i', '<M-f>', '<C-o>w', { silent = true })
map('i', '<M-b>', '<C-o>b', { silent = true })
map('i', '<M-d>', '<C-o>dw', { silent = true })

-- Edit
map({ 'n', 'v' }, '<leader>es', ':sort<CR>', { desc = 'Sort lines' })
map({ 'n', 'v' }, '<leader>e<space>', ':StripTrailingWhitespace<CR>', { desc = 'Strip whitespace' })

-- Global
map('n', '<leader><space>', '<cmd>Telescope fd<CR>', { silent = true, desc = 'Files' })
map('n', '<leader>*', '<cmd>Telescope grep_string<CR>', { silent = true, desc = 'Symbol' })
map('n', '<leader>,', '<cmd>Telescope buffers<CR>', { silent = true, desc = 'Buffers' })
map('n', '<leader>.', '<cmd>Telescope file_browser<CR>', { silent = true, desc = 'File browser' })
map('n', '<leader>/', '<cmd>Telescope live_grep<CR>', { silent = true, desc = 'Grep' })

-- Buffers
map('n', '<leader>bb', '<cmd>Telescope buffers<CR>', { silent = true, desc = 'Buffers' })

-- Files
map('n', '<leader>fn', '<cmd>enew<CR>', { silent = true, desc = 'New' })
map('n', '<leader>ff', '<cmd>Telescope fd<CR>', { silent = true, desc = 'Files' })
map('n', '<leader>fr', '<cmd>Telescope oldfiles<CR>', { silent = true, desc = 'Recent files' })
map('n', '<leader>fa', '<cmd>Telescope fd follow=true no_ignore=true hidden=true<CR>',
  { silent = true, desc = 'All files' })
map('n', '<leader>fb', '<cmd>Telescope file_browser<CR>', { silent = true, desc = 'Browse' })

-- Git
map('n', '<leader>gg', '<cmd>Neogit<CR>', { silent = true, desc = 'Status' })
map('n', '<leader>gf', '<cmd>Telescope git_files<CR>', { silent = true, desc = 'Files' })
map('n', '<leader>gB', '<cmd>Telescope git_branches<CR>', { silent = true, desc = 'Branches' })
map('n', '<leader>gC', '<cmd>Telescope git_commits<CR>', { silent = true, desc = 'Commits' })

-- Open
map('n', '<leader>ol', '<cmd>lopen<CR>', { silent = true, desc = 'Loclist' })
map('n', '<leader>oq', '<cmd>copen<CR>', { silent = true, desc = 'Quickfix' })

-- Search
map('n', '<leader>s:', '<cmd>Telescope command_history<CR>', { silent = true, desc = 'Commands' })
map('n', '<leader>sr', '<cmd>Telescope registers<CR>', { silent = true, desc = 'Registers' })
map('n', '<leader>sT', '<cmd>Telescope tags<CR>', { silent = true, desc = 'All tags' })
map('n', '<leader>sl', '<cmd>Telescope loclist<CR>', { silent = true, desc = 'Loclist' })
map('n', '<leader>sl', '<cmd>Telescope marks<CR>', { silent = true, desc = 'Marks' })
map('n', '<leader>sq', '<cmd>Telescope quickfix<CR>', { silent = true, desc = 'Quicfix' })
map('n', '<leader>ss', '<cmd>Telescope current_buffer_fuzzy_find<CR>', { silent = true, desc = 'Buffer' })
map('n', '<leader>st', '<cmd>Telescope current_buffer_tags<CR>', { silent = true, desc = 'Tags' })
map('n', '<leader>sS', '<cmd>Telescope live_grep grep_open_files=true<CR>', { silent = true, desc = 'All buffers' })

-- Toggle
map('n', '<leader>tt', '<cmd>Telescope themes<CR>', { silent = true, desc = 'Themes' })

-- Vim
-- TODO: open telescope with ~/.config/nvim

-- Windows
map('n', '<leader>w-', '<C-w>s', { silent = true, desc = "Split" })
map('n', '<leader>w<bar>', '<C-w>v', { silent = true, desc = "Vertical split" })
map('n', '<leader>w=', '<C-w>=', { silent = true, desc = "Balance" })
map('n', '<leader>wH', '<C-w>H', { silent = true, desc = "Move to left" })
map('n', '<leader>wJ', '<C-w>J', { silent = true, desc = "Move to bottom" })
map('n', '<leader>wK', '<C-w>K', { silent = true, desc = "Move to top" })
map('n', '<leader>wL', '<C-w>L', { silent = true, desc = "Move to right" })
-- TODO: maximize

--
-- Global
--

-- copy and paste
map({ 'n', 'x' }, '<M-w>', '"+y', { silent = true })
map('n', '<M-w><M-w>', '"+yy', { silent = true })
map({ 'n', 'x', 'i' }, '<M-y>', '<cmd>Telescope neoclip<CR>', { silent = true })

-- move linewise
map('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
map('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- unimpaired
map('n', '[a', '<cmd>previous<CR>', { desc = 'Previous file' })
map('n', ']a', '<cmd>next<CR>', { desc = 'Next file' })
map('n', '[b', '<cmd>bprevious', { desc = 'Previous buffer' })
map('n', ']b', '<cmd>bnext', { desc = 'Next buffer' })
map('n', '[l', '<cmd>lprevious', { desc = 'Previous loclist' })
map('n', ']l', '<cmd>lnext', { desc = 'Next loclist' })
map('n', '[q', '<cmd>cprevious', { desc = 'Previous quickfix' })
map('n', ']q', '<cmd>cnext', { desc = 'Next quickfix' })
map('n', '[d', vim.diagnostic.goto_prev, { desc = 'Previous error' })
map('n', ']d', vim.diagnostic.goto_next, { desc = 'Next error' })
map('n', '[o', "<cmd>put!=repeat([''],v:count)<bar>']+1<CR>", { desc = "Open below", silent = true })
map('n', ']o', "<cmd>put =repeat([''],v:count)<bar>'[-1<CR>", { desc = "Open above", silent = true })

-- other window
map('n', '<C-w><C-w>', '<C-w>w', { silent = true })

-- spelling suggestions
map('n', 'z=', '<cmd>Telescope spell_suggest', { silent = true })

-- exit term insert mode
map('t', 'jk', '<C-\\><C-n>', { silent = true })

-- quickly move around
map({ 'n', 'v', 'o' }, 's', '<cmd>Pounce<CR>')
map('n', 'S', '<cmd>PounceRepeat<CR>')

-- Make `q:` do nothing instead of opening command-line-window, because it is
-- often hit by accident
-- Use c_CTRL-F or Telescope
map('n', 'q:', '<Nop>')

-- search visually highlighted text
map('v', '<M-/>', [[y/\V<C-R>=escape(@",'/\')<CR><CR>]])

-- stop highlighting of search results
map('n', [[//]], [[:nohlsearch<C-R>=has('diff')?'<BAR>diffupdate':''<CR><CR>]])

-- delete selection in Select mode (helpful when editing snippet placeholders)
map('s', [[<BS>]], [[<BS>i]])

-- move inside completion list with tab
map('i', [[<Tab>]], [[pumvisible() ? "\<C-n>" : "\<Tab>"]], { expr = true })
map('i', [[<S-Tab>]], [[pumvisible() ? "\<C-p>" : "\<S-Tab>"]], { expr = true })

-- tmux integration
M.set_tmux_integration = function()
  map('n', '<M-h>', require('tmux').move_left, { silent = true })
  map('n', '<M-j>', require('tmux').move_bottom, { silent = true })
  map('n', '<M-k>', require('tmux').move_top, { silent = true })
  map('n', '<M-l>', require('tmux').move_right, { silent = true })
  map('n', '<M-S-h>', require('tmux').resize_left, { silent = true })
  map('n', '<M-S-j>', require('tmux').resize_bottom, { silent = true })
  map('n', '<M-S-k>', require('tmux').resize_top, { silent = true })
  map('n', '<M-S-l>', require('tmux').resize_right, { silent = true })
end

return M
