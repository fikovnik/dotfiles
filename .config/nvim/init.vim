" PLUGINS {{{
call plug#begin()
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'branch': '0.5-compat', 'do': ':TSUpdate'}
Plug 'folke/which-key.nvim'
Plug 'hrsh7th/nvim-compe'
Plug 'phaazon/hop.nvim'
Plug 'tpope/vim-commentary'
Plug 'navarasu/onedark.nvim'
Plug 'TimUntersberger/neogit'
Plug 'christoomey/vim-tmux-navigator'
Plug 'lervag/wiki.vim'
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
Plug 'lewis6991/gitsigns.nvim'
Plug 'hrsh7th/vim-vsnip'
Plug 'lervag/vimtex'
Plug 'https://github.com/lambdalisue/suda.vim/' " workaround for https://github.com/neovim/neovim/issues/1716
Plug 'preservim/vimux'
Plug 'neovim/nvim-lspconfig'
Plug 'szw/vim-maximizer'
Plug 'AckslD/nvim-neoclip.lua'
Plug 'tpope/vim-fugitive'
Plug 'dyng/ctrlsf.vim'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install'  }
Plug 'tpope/vim-sleuth'
Plug 'junegunn/vim-easy-align'
Plug 'mfussenegger/nvim-dap'
Plug 'simrat39/rust-tools.nvim'
Plug 'scalameta/nvim-metals'
call plug#end()
" }}}

" BASICS {{{
" settings {{{
set autoindent
set autoread " auto read changes if a buffer was modified outside of vim
set autowrite " auto write changes after certain commands
set backspace=start,eol,indent " allow backspace to delete all
set backup
set backupdir=$HOME/.cache/vim/backup
set breakindent " wrap lines continue visually indented
set cmdheight=2 " give more space for displaying messages
set completeopt=menuone,noselect " nvim-compe requirement
set copyindent " copy the previous indentation on autoindenting
set cursorline
set encoding=utf-8
set expandtab " copy the structure of existing lines
set exrc " allow per directory rc files
set fileencoding=utf-8
set fileencodings=utf-8
set foldopen=insert,mark,percent,quickfix,search,tag,undo
set grepprg=rg\ --vimgrep\ --smart-case\ --follow
set hidden " allow to switch buffers when there are unsaved changes
set ignorecase " case insensitive search
set inccommand=nosplit " show replacements inplace
set infercase
set lazyredraw " do not update screen when running macros
set linebreak " break on word end
set nocompatible
set nojoinspaces " do not add extra space when joining lines
set noswapfile
set number
set path+=** " find files in subdirectories
set preserveindent " preserve the current file indent as much as possible
set scrolloff=10 " leave 10 lines up and down when scrolling for context
set secure " disable certain commands in exrc
set shiftwidth=2
set shortmess+=c " don't pass messages to |ins-completion-menu|
set showcmd
set showmatch " briefly jump to the matching object
set signcolumn=yes
set smartcase " override ignorecase if pattern contains capital letter
set softtabstop=-1 " use shiftwidth instead
set spelllang=en,cs,csa " use multiple languages at the same time
set splitbelow " split: swap order so the text does not move
set splitright " vsplit: swap order so the text does not move
set tabstop=2
set tagcase=match " match case when searching tags
set thesaurus=$HOME/.config/nvim/spell/thesaurus.txt
set timeoutlen=500 " timeout for which-key
set title
set undodir=$HOME/.cache/vim/undo
set undofile
set updatetime=100 " faster update time (default is 4s)
set whichwrap+=<,>,h,l,[,] " move past new line ends
set wildignore+=*.class
set wildignore+=*.o
set wildignore+=*.swp,~*
set wildignore+=*.zip,*.tar
set wildignore+=.git/*
set wrap
" }}}

" create directories {{{
if !isdirectory(&undodir)
  call mkdir(&undodir, 'p')
endif
if !isdirectory(&backupdir)
  call mkdir(&backupdir, 'p')
endif
" }}}

" common {{{
filetype plugin indent on
syntax on

" check for changes on focus gain
au FocusGained,BufEnter * :checktime

" remember cursor position
augroup remember_cursor_position
  autocmd!
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe            "normal! g`\"" | endif
augroup END

" highlight yank operations
augroup highlight_yank
  au!
  au TextYankPost * lua vim.highlight.on_yank{ higroup="IncSearch", timeout=250 }
augroup END

" set leaders
let g:mapleader = "\<Space>"
let g:maplocalleader = ','
" }}}
" }}}

" KEYS {{{
" buffers {{{
nmap <silent> <leader>bR <cmd>SudaRead<CR>
nmap <silent> <leader>bW <cmd>SudaWrite<CR>
nmap <silent> <leader>bb <cmd>TS buffers<CR>
nnoremap <silent> <leader>bd <cmd>bd<CR>
nnoremap <silent> <leader>bn <cmd>bn<CR>
nnoremap <silent> <leader>bp <cmd>bp<CR>

function! MyDiffBufferFile()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction

command! MyDiffBufferFile :call MyDiffBufferFile()

nmap <silent> <leader>bD <cmd>MyDiffBufferFile<CR>
" }}}
" completion {{{
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-g>     compe#close('<C-g>')
inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })
" }}}
" edit {{{
" list yanks
nnoremap <silent> <M-y> <cmd>TS neoclip<CR>
inoremap <silent> <M-y> <cmd>TS neoclip<CR>
xnoremap <silent> <M-y> <cmd>TS neoclip<CR>
" copy to system clipboard (neovim does not support yet C-S-c)
nnoremap <silent> <C-y> "+y
nnoremap <silent> <C-y><C-y> "+yy
xnoremap <silent> <C-y> "+y
" xnoremap <silent> <C-S-c> "+y
" insert new lines
nnoremap <silent> [o  <cmd>put!=repeat([''],v:count)<bar>']+1<CR>
nnoremap <silent> ]o  <cmd>put =repeat([''],v:count)<bar>'[-1<CR>
" use C-\ to insert a digraph
inoremap <C-\> <C-k>
" format paragraph
imap <M-q> <C-o><M-q>
nmap <M-q> <cmd>call MyFormatParagraph()<CR>
vnoremap <M-q> gq
vnoremap <silent> <leader>es :sort<CR>
" snippets
nmap <silent> <leader>eSs <cmd>TS ultisnips<CR>
nmap <silent> <leader>eSe <cmd>UltiSnipsEdit<CR>
" TODO allow line-wise movement
" - the following breaks the code completion in insert mode
" inoremap <down> <c-\><c-o>gj
" inoremap <up> <c-\><c-o>gk

" align
xmap <leader>ea <Plug>(EasyAlign)
nmap <leader>ea <Plug>(EasyAlign)

function! MyFormatParagraph()
  let pos = getcurpos()
  silent execute 'normal! gqap'
  call setpos('.', pos)
endfunction
" }}}
" emacs-style {{{
noremap <C-g> <Esc>
vnoremap <C-g> <Esc>
cnoremap <C-g> <Esc>
cnoremap <C-BS> <C-W>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-k> <C-\>estrpart(getcmdline(),0,getcmdpos()-1)<CR>
snoremap <C-g> <Esc>
inoremap <C-g> <Esc>
inoremap <C-e> <C-o>$
inoremap <C-a> <C-o>^
inoremap <C-BS> <C-W>
inoremap <C-k> <C-o>d$
" }}}
" file {{{
nmap <silent> <leader>fn <cmd>enew<CR>
nmap <silent> <leader>ff <cmd>TS file_browser<CR>
nmap <silent> <leader>fr <cmd>TS oldfiles<CR>
" }}}
" git {{{
nmap <silent> <leader>gC <cmd>TS git_commits<CR>
nmap <silent> <leader>gb <cmd>TS git_branches<CR>
nmap <silent> <leader>gc <cmd>TS git_bcommits<CR>
nmap <silent> <leader>gf <cmd>TS git_files<CR>
nmap <silent> <leader>gg <cmd>Neogit<CR>
nmap <silent> <leader>ghp <cmd>lua require"gitsigns".preview_hunk()<CR>
nmap <silent> <leader>ghr <cmd>lua require"gitsigns".reset_hunk()<CR>
nmap <silent> <leader>ghs <cmd>lua require"gitsigns".stage_hunk()<CR>
nmap <silent> <leader>ghu <cmd>lua require"gitsigns".undo_stage_hunk()<CR>
nmap <silent> <leader>gl <cmd>lua require"gitsigns".blame_line(true)<CR>
nmap <silent> <leader>gr <cmd>lua require"gitsigns".reset_buffer()<CR>
nmap <silent> <leader>gs <cmd>TS git_stash<CR>
vmap <silent> <leader>ghr <cmd>lua require"gitsigns".reset_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>
vmap <silent> <leader>ghs <cmd>lua require"gitsigns".stage_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>
" }}}
" global {{{
nmap <silent> <leader>' <cmd>TS search_history<CR>
nmap <silent> <leader>* <cmd>TS grep_string<CR>
nmap <silent> <leader>, <cmd>TS buffers<CR>
nmap <silent> <leader>. <cmd>TS file_browser<CR>
nmap <silent> <leader>/ <cmd>TS live_grep<CR>
nmap <silent> <leader><space> <cmd>TS find_files<CR>
" }}}
" help {{{
nmap <silent> <leader>hH <cmd>TS highlights<CR>
nmap <silent> <leader>hc <cmd>TS commands<CR>
nmap <silent> <leader>hh <cmd>TS help_tags<CR>
nmap <silent> <leader>hk <cmd>TS keymaps<CR>
nmap <silent> <leader>hm <cmd>TS man_pages<CR>
" }}}
" lsp {{{
nmap <silent> <leader>lSi <cmd>LspInfo<CR>
nmap <silent> <leader>lSr <cmd>LspRestart<CR>
nmap <silent> <leader>lSs <cmd>LspStart<CR>
nmap <silent> <leader>lSk <cmd>LspStop<CR>
" other keys are set using onattach in LSP
" }}}
" navigation {{{
" an alternative to C-i/C-o because nvim currently cannot map
" tab and C-i to different keys
nnoremap <silent> <M-Right> <cmd>execute "normal 1\<C-I>"<CR>
nnoremap <silent> <M-Left> <cmd>execute "normal 1\<C-O>"<CR>
" move across line boundaries
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
xnoremap <expr> j v:count ? 'j' : 'gj'
xnoremap <expr> k v:count ? 'k' : 'gk'
" hopping
nmap <silent> gy <cmd>HopWord<CR>
nmap <silent> gl <cmd>HopLine<CR>
nmap <silent> gs <cmd>HopChar1<CR>
nmap <silent> gS <cmd>HopChar2<CR>
" }}}
" notes {{{
nmap <silent> <leader>ni <cmd>WikiIndex<CR>
nmap <silent> <leader>njj <cmd>WikiJournal<CR>
nmap <silent> <leader>njn <cmd>WikiJournalNext<CR>
nmap <silent> <leader>njp <cmd>WikiJournalPrev<CR>
nmap <silent> <leader>nn <cmd>WikiFzfPages<CR>
nmap <silent> <leader>ns <cmd>execute 'TS' 'live_grep' 'search_dirs=' . fnameescape(g:wiki_root)<CR>
" }}}
" open {{{
nnoremap <silent> <leader>o- <cmd>Explore<CR>
nnoremap <silent> <leader>ol <cmd>lopen<CR>
nnoremap <silent> <leader>oq <cmd>copen<CR>
" }}}
" search {{{
nmap <silent> <leader>s: <cmd>TS command_history<CR>
nmap <silent> <leader>sR <cmd>TS registers<CR>
nmap <silent> <leader>sT <cmd>TS tags<CR>
nmap <silent> <leader>sl <cmd>TS loclist<CR>
nmap <silent> <leader>sm <cmd>TS marks<CR>
nmap <silent> <leader>sq <cmd>TS quickfix<CR>
nmap <silent> <leader>sr <Plug>CtrlSFPrompt
vmap <silent> <leader>sr <Plug>CtrlSFVwordPath
nmap <silent> <leader>ss <cmd>TS current_buffer_fuzzy_find<CR>
nmap <silent> <leader>st <cmd>TS current_buffer_tags<CR>
" }}}
" spell {{{
nnoremap z= <cmd> TS spell_suggest<CR>
" }}}
" terminal {{{
tnoremap jk <C-\><C-n>
" }}}
" toggle {{{
nmap <silent> <leader>tb <cmd>Gitsigns toggle_current_line_blame<CR>
nmap <silent> <leader>tg <cmd>Gitsigns toggle_signs<CR>
nmap <silent> <leader>th <cmd>set hls!<CR>
nmap <silent> <leader>tp <cmd>setlocal paste!<CR>
nmap <silent> <leader>ts <cmd>setlocal spell!<CR>
nmap <silent> <leader>tw <cmd>setlocal wrap!<CR>
" }}}
" vim {{{
nmap <silent> <leader>ve <cmd>edit ~/.config/nvim/init.vim<CR>
nmap <silent> <leader>vfs <cmd>syntax sync fromstart<CR>
nmap <silent> <leader>vo <cmd>TS vim_options<CR>
nmap <silent> <leader>vpU <cmd>PlugUpgrade<CR>
nmap <silent> <leader>vpc <cmd>PlugClean<CR>
nmap <silent> <leader>vpi <cmd>PlugInstall<CR>
nmap <silent> <leader>vps <cmd>PlugStatus<CR>
nmap <silent> <leader>vpu <cmd>PlugUpdate<CR>
nmap <silent> <leader>vq <cmd>quitall<CR>
nmap <silent> <leader>vs <cmd>update!<CR>:<C-u>source ~/.config/nvim/init.vim<CR>:<C-u>echo "Sourced!"<CR>
" }}}
" vimux {{{
nmap <C-c><C-c> vip<leader>cs
nmap <C-c><C-l> V<leader>cs
nmap <leader>cS V<leader>cs<CR>
nmap <leader>cd <cmd>VimuxCloseRunner<CR>
nmap <leader>ci <cmd>VimuxInspectRunner<CR>
nmap <leader>cl <cmd>VimuxClearTerminalScreen<CR>
nmap <leader>co <cmd>VimuxPromptCommand<CR>
nmap <silent> <leader>cs :set opfunc=MyVimuxSlimeMotion<CR>g@
nmap <leader>cx <cmd>VimuxInterruptRunner<CR>
nmap <leader>cz <cmd>call VimuxZoomRunner()<CR>
vmap <C-c><C-c> <leader>cs
vmap <leader>cs "vy:call MyVimuxSlime()<CR>
" }}}
" windows {{{
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <C-w><C-w> <C-w>w
nnoremap <leader>w- <C-w>s
nnoremap <leader>w<bar> <C-w>v
nnoremap <leader>w= <C-w>=
nnoremap <leader>wH <C-w>H
nnoremap <leader>wJ <C-w>J
nnoremap <leader>wK <C-w>K
nnoremap <leader>wL <C-w>L
nnoremap <leader>wk <C-w>c
nnoremap <silent> <leader>wm <cmd>MaximizerToggle<CR>
vnoremap <silent> <leader>wm <cmd>MaximizerToggle<CR>gv
" }}}
" }}}

" UI {{{
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

let g:disable_toggle_style = 1 " disable the <leade>cs shortcut
colorscheme onedark

hi Visual guibg=#264f78
hi MatchParen guifg=#e86671

" curly underline of misspelled words
hi SpellBad   cterm=undercurl gui=undercurl ctermfg=NONE guifg=NONE guisp=#e86671
hi SpellCap   cterm=undercurl gui=undercurl ctermfg=NONE guifg=NONE guisp=#e5c07b
hi SpellLocal cterm=undercurl gui=undercurl ctermfg=NONE guifg=NONE guisp=#e5c07b
hi SpellRare  cterm=undercurl gui=undercurl ctermfg=NONE guifg=NONE guisp=#e5c07b
" }}}

" LSP {{{
lua << EOF
local util = require('lspconfig/util')
local lsp = require('lspconfig')
capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    'documentation',
    'detail',
    'additionalTextEdits',
  }
}

on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap=true, silent=true }

  buf_set_keymap('n', '<leader>lE', '<cmd>TS lsp_workspace_diagnostics<CR>', opts)
  buf_set_keymap('n', '<leader>lL', '<cmd>TS lsp_dynamic_workspace_symbols<CR>', opts)
  buf_set_keymap('n', '<leader>la', '<cmd>TS lsp_code_actions<CR>', opts)
  buf_set_keymap('n', '<leader>ld', '<cmd>TS lsp_definitions<CR>', opts)
  buf_set_keymap('n', '<leader>le', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '<leader>lf', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
  buf_set_keymap('n', '<leader>lh', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', '<leader>li', '<cmd>TS lsp_implementations<CR>', opts)
  buf_set_keymap('n', '<leader>ll', '<cmd>TS lsp_document_symbols<CR>', opts)
  buf_set_keymap('n', '<leader>lr', '<cmd>TS lsp_references<CR>', opts)
  buf_set_keymap('n', '<leader>ls', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<leader>lt', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<leader>lwa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>lwl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<leader>lwr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', '[e', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']e', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('v', '<leader>la', '<cmd>TS lsp_range_code_actions<CR>', opts)
end

lsp.r_language_server.setup { 
  filetypes = { "R", "r", "Rmd" },
  root_dir = util.root_pattern(".git") or cwd,
  capabilities = capabilities,
  on_attach = on_attach,
}

lsp.clangd.setup {
  capabilities = capabilities,
  on_attach = on_attach,
}

lsp.hls.setup {
  on_attach = on_attach,
}
EOF
" }}}

" plugin: ctrlsf {{{
let g:ctrlsf_auto_close = {
    \ "normal" : 0,
    \ "compact": 0
    \}
let g:ctrlsf_auto_focus = {
    \ "at": "start"
    \ }
let g:ctrlsf_mapping = {
    \ "next": "n",
    \ "prev": "N",
    \ }
" }}}

" plugin: hop {{{
lua << EOF
require('hop').setup()
EOF
" }}}

" plugin: gitsigns {{{
lua << EOF
require('gitsigns').setup {
  keymaps = {
    ['n ]c'] = { expr = true, "&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'"},
    ['n [c'] = { expr = true, "&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'"},
    ['o ih'] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>',
    ['x ih'] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>'
  }
}
EOF
" }}}

" plugin: neoclip {{{
lua << EOF
require('neoclip').setup({
  content_spec_colunm = true,
  preview = false,
})
EOF
" }}}

" plugin: neogit {{{
lua << EOF
require('neogit').setup()
EOF
" }}}

" plugin: nvim-compe {{{
lua << EOF
require('compe').setup {
  enabled = true,
  autocomplete = true,
  min_length = 2,
  source = {
    path = true,
    buffer = true,
    nvim_lsp = true,
    vsnip = true,
    omni = {
      filetypes = {'tex'},
    },
  },
}
EOF
" }}}

" plugin: pandoc {{{ 
let g:pandoc#keyboard#use_default_mappings = 0
let g:pandoc#syntax#conceal#urls = 1
let g:pandoc#syntax#codeblocks#embeds#langs = ["scala", "literatehaskell=lhaskell", "bash=sh"]
let g:pandoc#syntax#conceal#blacklist = [ "atx", "list" ]
" }}}

" plugin: rust-tools {{{
lua << EOF

require('rust-tools').setup {
  server = {
    capabilities = capabilities,
    on_attach = on_attach,
    settings = {
      ["rust-analyzer"] = {
        checkOnSave = {
          command = "clippy"
        },
      },
    },
  },
}

EOF

augroup my-rust
  au!
  au FileType rust call MySetupRust()
augroup end

function! MySetupRust() 
  nmap <buffer><silent> <localleader>D <cmd>RustDebuggables<CR>
  nmap <buffer><silent> <localleader>J <cmd>RustJoinLines<CR>
  nmap <buffer><silent> <localleader>R <cmd>RustRunnables<CR>
  nmap <buffer><silent> <localleader>co <cmd>RustOpenCargo<CR>
  nmap <buffer><silent> <localleader>cr <cmd>CargoReload<CR>
  nmap <buffer><silent> <localleader>cr <cmd>CargoReload<CR>
  nmap <buffer><silent> <localleader>e <cmd>RustExpandMacro<CR>
  nmap <buffer><silent> <localleader>h <cmd>RustHoverActions<CR>
  nmap <buffer><silent> <localleader>j <cmd>RustMoveItemDown<CR>
  nmap <buffer><silent> <localleader>k <cmd>RustMoveItemUp<CR>
  nmap <buffer><silent> <localleader>p <cmd>RustParentModule<CR>
  nmap <buffer><silent> <localleader>r <cmd>RustRun<CR>
  nmap <buffer><silent> <localleader>t <cmd>RustHoverRange<CR>
  nmap <buffer><silent> <localleader>th <cmd>RustToggleInlayHints<CR>
endfunction
" }}}

" plugin: scala-metals {{{
lua << EOF

metals = require("metals")
metals_config = metals.bare_config
metals_config.settings = {
  showImplicitArguments = true,
  showInferredType = true,
}
metals_config.capabilities = capabilities
metals_config.on_attach = on_attach
metals_config.init_options.statusBarProvider = "on"

vim.cmd([[augroup my-scala]])
vim.cmd([[autocmd!]])
vim.cmd([[autocmd FileType scala,sbt lua metals.initialize_or_attach(metals_config)]])
vim.cmd([[augroup end]])

EOF
" }}}

" plugin: telescope {{{
lua <<EOF
local ts = require('telescope')

ts.setup {
  dynamic_preview_title = true,
}

ts.load_extension('neoclip')
EOF

command -nargs=* TS Telescope <args> theme=get_ivy
" }}}

" plugin: treesitter {{{
lua <<EOF
require('nvim-treesitter.configs').setup {
  ensure_installed = "maintained",
  ignore_install = { "latex" },
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<M-=>",
      node_incremental = "<M-=>",
      scope_incremental = "<M-+>",
      node_decremental = "<M-->",
    },
  },
}
EOF
" }}}

" plugin: vim-tmux-navigator {{{
let g:tmux_navigator_save_on_switch = 2
let g:tmux_navigator_disable_when_zoomed = 1
let g:tmux_navigator_no_mappings = 1

tnoremap <silent> <M-h> <C-\><C-n>:TmuxNavigateLeft<cr>
tnoremap <silent> <M-j> <C-\><C-n>:TmuxNavigateDown<cr>
tnoremap <silent> <M-k> <C-\><C-n>:TmuxNavigateUp<cr>
tnoremap <silent> <M-l> <C-\><C-n>:TmuxNavigateRight<cr>
tnoremap <silent> <M-\> <C-\><C-n>:TmuxNavigatePrevious<cr>
nnoremap <silent> <M-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <M-j> :TmuxNavigateDown<cr>
nnoremap <silent> <M-k> :TmuxNavigateUp<cr>
nnoremap <silent> <M-l> :TmuxNavigateRight<cr>
nnoremap <silent> <M-\> :TmuxNavigatePrevious<cr>
" }}}

" plugin: vimtex {{{
let g:vimtex_format_enabled = 1
let g:vimtex_quickfix_mode = 0
let g:vimtex_quickfix_open_on_warning = 0
let g:vimtex_quickfix_ignore_filters = [
  \ 'Overfull',
  \ 'Underfull',
  \ ]
let g:vimtex_view_general_viewer = '/usr/bin/sioyek' 
let g:vimtex_view_general_options = '--inverse-search "nvr --remote-expr \"vimtex#view#reverse_goto(%2, ''%1'')\"" --reuse-instance --forward-search-file @tex --forward-search-line @line @pdf' 
let g:vimtex_view_general_options_latexmk = '--reuse-instance'
" }}}

" plugin: vimux {{{
function! MyVimuxSlime()
 call VimuxRunCommand(@v)
endfunction

function! MyVimuxSlimeMotion(type)
    silent execute 'normal! `[v`]"vy'    
    call MyVimuxSlime()
endfunction

let g:VimuxCloseOnExit = 0
let g:VimuxUseNearest = 1
" }}}

" plugin: vim-vsnip {{{
let g:vsnip_snippet_dir = expand('~/.config/nvim/snippets/')

imap <expr> <Tab>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<Tab>'
smap <expr> <Tab>   vsnip#jumpable(1)  ? "<Plug>(vsnip-jump-next)" : "<Tab>"
smap <expr> <S-Tab> vsnip#jumpable(-1) ? "<Plug>(vsnip-jump-prev)" : "<S-Tab>"
xmap <Tab> <Plug>(vsnip-cut-text)
" }}}

" plugin: which-key {{{

lua << EOF
local wk = require("which-key")
wk.setup {
  key_labels = {
    ["<space>"] = "SPC",
    ["<CR>"] = "RET",
    ["<tab>"] = "TAB",
  }
}

wk.register({
  ["<leader>"] = {
   ["<space>"] = "Find file in project",
   ["/"] = "Search in project",
   ["."] = "Find file",
   [","] = "Switch buffer",
   ["*"] = "Search symbol in project",
   ["'"] = "Search history",
  },
  ["<leader>b"] = {
    name = "buffers",
    b = "Buffer list",
    n = "Next buffer",
    p = "Previous buffer",
    d = "Delete buffer",
    D = "Diff with file on disk",
    R = "Read as sudo",
    W = "Write as sudo",
  },
  ["<leader>c"] = {
    name = "console",
    S = "Send line",
    i = "Inspect",
    d = "Delete",
    l = "Clear",
    o = "Open",
    s = "Send paragraph",
    x = "Interrupt",
    z = "Zoom console",
  },
  ["<leader>e"] = {
    name = "edit",
    a = "Align",
    s = "Sort lines",
    S = {
      name = "snippets",
      s = "Snippets list",
      e = "Edit snippets",
    }
  },
  ["<leader>f"] = {
    name = "files",
    f = "Find file",
    n = "New file",
    r = "Recent file",
  }, 
  ["<leader>g"] = {
    name = "git",
    C = "Commits",
    b = "Branches",
    c = "Buffer commits",
    f = "Files",
    g = "Status",
    h = {
      name = "hunk",
      p = "Preview hunk",
      r = "Reset hunk",
      s = "Stage hunk",
      u = "Undo hunk stage",
    },
    l = "Blame line",
    r = "Reset buffer",
    s = "Stashes",
  },
  ["<leader>h"] = {
    name = "help",
    H = "Highlights",
    c = "Commands",
    h = "Help tags",
    k = "Keymaps",
    m = "Man pages",
  },
  ["<leader>l"] = {
    name = "lsp",
    E = "Errors in workspace",
    L = "Symbols in workspace",
    S = {
      name = "server",
      i = "Info",
      k = "Stop",
      r = "Restart",
      s = "Start",
    },
    a = "Actions",
    d = "Definitions",
    e = "Errors in buffer",
    f = "Format",
    h = "Hover",
    i = "Implementations",
    l = "Symbols in buffer",
    r = "References",
    s = "Signature help",
    t = "Type definition",
    w = "Workspace folders",
  },
  ["<leader>o"] = {
    name = "open",
    q = "Quickfix",
    l = "Locations",
    ["-"] = "Directory",
  },
  ["<leader>s"] = {
    name = "search",
    [":"] = "Command history",
    R = "Registers",
    T = "Tags",
    l = "Locations",
    m = "Marks",
    q = "Quickfix",
    r = "Replace in project",
    s = "Current buffer",
    t = "Current buffer tags",
  },
  ["<leader>t"] = {
    name = "toggle",
    b = "Blame current line",
    g = "Git signs",
    h = "Highlight",
    p = "Paste",
    s = "Spell",
    w = "Wrap"
  },
  ["<leader>v"] = {
    name = "vim",
    e = "Edit init.vim",
    s = "Source init.vim",
    f = {
      name = "fix",
      s = "Syntax",
    },
    o = "Options",
    p = {
      name = "plugins",
      c = "Clean",
      i = "Install",
      s = "Status",
      u = "Update",
      U = "Upgrade",
    },
  },
  ["<leader>w"] = {
    name = "windows",
    ["-"] = "Split horizontaly",
    ["|"] = "Split vertically",
    ["="] = "Balance windows",
    ["H"] = "Move window to the left",
    ["J"] = "Move window to the bottom",
    ["K"] = "Move window to the top",
    ["L"] = "Move window to the right",
    ["c"] = "Close window",
  },
  ["g"] = {
    c = "Comment",
    l = "Hop line",
    s = "Hop 1 character",
    S = "Hop 2 characters",
    w = "Hop word",
  },
})
EOF
" }}}

" plugin: wiki {{{
let g:wiki_root = '~/Notes'
let g:wiki_zotero_root = '~/Research/Resources/Zotero'
let g:wiki_filetypes = ['md']
let g:wiki_link_extension = '.md'
let g:wiki_link_target_type = 'md'
let g:wiki_journal = {
  \ 'name': 'Journal',
  \ 'frequency': 'weekly',
  \ 'date_format': {
  \   'daily' : '%Y-%m-%d',
  \   'weekly' : '%Y-w%V',
  \   'monthly' : '%Y-w%m',
  \ },
\}
let g:wiki_mappings_use_defaults = 'none'
let g:wiki_list_todos = ['TODO', 'DONE', 'WAIT']
let g:wiki_template_title_week = '# Week %(week) in %(year)'
let g:wiki_export = {
  \ 'args' : '--self-contained --template github',
  \ 'from_format' : 'markdown',
  \ 'ext' : 'html',
  \ 'link_ext_replace': v:false,
  \ 'view' : v:true,
  \ 'output': 'Exported',
  \}

let g:wiki_mappings_local = {
  \ '<plug>(wiki-export)':               '<localleader>E',
  \ '<plug>(wiki-fzf-toc)':              '<localleader>,',
  \ '<plug>(wiki-link-follow)':          '<localleader>o',
  \ '<plug>(wiki-link-next)':            '<localleader>n',
  \ '<plug>(wiki-link-prev)':            '<localleader>p',
  \ '<plug>(wiki-link-return)':          '<localleader>b',
  \ '<plug>(wiki-list-toggle)':          '<localleader>l',
  \ '<plug>(wiki-page-delete)':          '<localleader>D',
  \ '<plug>(wiki-page-rename)':          '<localleader>R',
  \ 'x_<plug>(wiki-export)':             '<localleader>E',
  \ 'x_<plug>(wiki-link-toggle-visual)': '<localleader><cr>',
  \ }
" }}}

" file-type: latex {{{
augroup my-latex
  au!
  au FileType latex nmap <buffer><silent> <localleader>lt <cmd>call vimtex#fzf#run()<CR>
  au FileType latex vmap <buffer><silent> <localleader>lf :!latexindent -m -l -<CR>
augroup end
" }}}

" file-type: markdown {{{
augroup my-markdown
  au!
  au FileType markdown call MySetupMarkdown()
  au FileType pandoc call MySetupMarkdown()
augroup end

function! MyMdCodeBlockTextObj(type) abort
  let start_row = searchpos('\s*```', 'bnW')[0]
  let end_row = searchpos('\s*```', 'nW')[0]

  let buf_num = bufnr()
  if a:type ==# 'i'
    let start_row += 1
    let end_row -= 1
  endif

  execute 'normal! ' start_row . 'G|V|' . end_row . 'G'
endfunction

function! MySetupMarkdown()
  setlocal spell
  setlocal conceallevel=2
  setlocal foldlevel=1
  setlocal tabstop=4
  vmap <buffer><silent> ib :<C-U>call MyMdCodeBlockTextObj('i')<CR>
  omap <buffer><silent> ib :normal vib<CR>
  vmap <buffer><silent> ab :<C-U>call MyMdCodeBlockTextObj('a')<CR>
  omap <buffer><silent> ab :normal vab<CR>
  nmap <buffer><silent> <C-c><C-c> vib<leader>cs
  nmap <buffer><silent> <leader>tP <Plug>MarkdownPreviewToggle
  nmap <buffer><silent> <localleader>tf <leader>eaip*\|
endfunction
" }}}

" file-type: vim {{{
augroup my-vim
  au!
  au FileType vim setlocal foldmethod=marker
augroup end
" }}}
