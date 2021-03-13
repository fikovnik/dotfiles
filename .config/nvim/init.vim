" PLUGINS {{{
"
" note: make sure you use single quotes
call plug#begin('~/.config/nvim/plugged')
" completion of all sorts of things
Plug 'junegunn/fzf.vim'
" let other people worry about number tabs and spaces
Plug 'editorconfig/editorconfig-vim'
" allow plugins to bin to the repeat command
Plug 'tpope/vim-repeat'
" git
Plug 'tpope/vim-fugitive'
" move lines
Plug 't9md/vim-textmanip'
" better matching
Plug 'andymass/vim-matchup'
" simple code comment
Plug 'tpope/vim-commentary'
" more targets
Plug 'wellle/targets.vim'
" markdown editing support
Plug 'plasticboy/vim-markdown'
" notes support
Plug 'lervag/wiki.vim'
" latex
Plug 'lervag/vimtex'
" git info in the gutter
Plug 'airblade/vim-gitgutter'
" LSP support
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" FZF support fro COC
Plug 'antoinemadec/coc-fzf'
" theme
Plug 'joshdick/onedark.vim'
" status line
Plug 'itchyny/lightline.vim'
" keys
Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!', 'WhichKeyVisual', 'WhichKeyVisual!'] }
" surround
Plug 'machakann/vim-sandwich'
" snippets
Plug 'SirVer/ultisnips'
" actual snippets
Plug 'honza/vim-snippets'
" moving around
Plug 'easymotion/vim-easymotion'
" find project roots
Plug 'airblade/vim-rooter'
" get rid of trailing whitespace
Plug 'axelf4/vim-strip-trailing-whitespace'
" align text
Plug 'junegunn/vim-easy-align'
" rust
Plug 'rust-lang/rust.vim'
" slime
Plug 'jpalardy/vim-slime'
call plug#end()
" }}}

" OPTIONS {{{

" allow to switch buffers when there are unsaved changes
set hidden
" auto write changes after certain commands
set autowrite
" give more space for displaying messages
set cmdheight=2
set nocompatible
" faster update time (default is 4s)
set updatetime=100
set timeoutlen=500
set cursorline
" spell
set spell
set thesaurus=$HOME/.config/nvim/spell/thesaurus.txt
" line numbers
set relativenumber
set number
" don't pass messages to |ins-completion-menu|
set shortmess+=c
set signcolumn=yes
" use rg for grepping
set grepprg=rg\ --vimgrep\ --smart-case\ --follow
" do not update screen when running macros
set lazyredraw
" ignore more files
set wildignore+=.git/*
set wildignore+=*.o
set wildignore+=*.class
set wildignore+=*.swp,~*
set wildignore+=*.zip,*.tar
" save undo
set undofile
set undodir=$HOME/.cache/vim/undo
" do backup
set backup
set backupdir=$HOME/.cache/vim/backup
" no swap
set noswapfile
" use shiftwidth
set softtabstop=-1
set shiftwidth=2
set tabstop=2
" use spaces to insert a tab
set expandtab
" copy the structure of existing lines
set copyindent
" preserve the current file indent as much as possible
set preserveindent
" wrap lines continue visually indented
set breakindent
" smartcase
set ignorecase
set smartcase
set infercase
" briefly jump to the matching object
set showmatch
" match case when searching tags
set tagcase=match
" vsplit/split: swap order so the text does not move
set splitbelow
set splitright
" find files in subdirectories
set path=$PWD/**
" only the following commands should open folds
set foldopen=insert,mark,percent,quickfix,search,tag,undo
set title
set wrap

" aet leaders
let g:mapleader = "\<Space>"
let g:maplocalleader = ','

" neovim specific options
if has('nvim')
  set inccommand=nosplit
endif

if !isdirectory(&undodir)
  call mkdir(&undodir, 'p')
endif
if !isdirectory(&backupdir)
  call mkdir(&backupdir, 'p')
endif

" }}}

" UI {{{
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

" let g:nord_italic = 1
" let g:nord_italic_comments = 1
" let g:nord_underline = 1
" let g:nord_uniform_status_lines = 1
" let g:nord_uniform_diff_background = 1
" let g:nord_cursor_line_number_background = 1

let g:onedark_terminal_italics = 1
let g:rainbow_active = 1

syntax on
set background=dark
" colorscheme nord
colorscheme onedark
" }}}

" STATUS LINE {{{
let g:lightline = {
  \ 'colorscheme': 'onedark',
  \ }
" }}}

" plugin git-gutter {{{
" do not set any mapping on its own
let g:gitgutter_highlight_linenrs = 1

nmap <leader>ghs <plug>(GitGutterStageHunk)
nmap <leader>ghu <plug>(GitGutterUndoHunk)
nmap <leader>ghp <plug>(GitGutterPreviewHunk)
" }}}

" LSP (COC) {{{
let g:coc_global_extensions = [
  \ 'coc-clangd',
  \ 'coc-dictionary',
  \ 'coc-json',
  \ 'coc-pairs',
  \ 'coc-r-lsp',
  \ 'coc-rust-analyzer',
  \ 'coc-tag',
  \ 'coc-ultisnips',
  \ 'coc-yank',
  \ 'coc-vimtex',
\]

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" leader-l mappings
nmap <silent><nowait> <leader>lD :<C-u>CocFzfList diagnostics<cr>
nmap <silent><nowait> <leader>lE :<C-u>CocFzfList extensions<cr>
nmap <silent><nowait> <leader>lC :<C-u>CocFzfList commands<cr>
nmap <silent><nowait> <leader>lO :<C-u>CocFzfList outline<cr>
nmap <silent><nowait> <leader>lS :<C-u>CocFzfList symbols<cr>
nmap <silent><nowait> <leader>lj :<C-u>CocNext<cr>
nmap <silent><nowait> <leader>lk :<C-u>CocPrev<cr>
nmap <silent><nowait> <leader>l' :<C-u>CocFzfListResume<cr>
nmap <silent>         <leader>ld <Plug>(coc-definition)
nmap <silent>         <leader>lt <Plug>(coc-type-definition)
nmap <silent>         <leader>li <Plug>(coc-implementation)
nmap <silent>         <leader>lr <Plug>(coc-references)
nmap                  <leader>lR <Plug>(coc-rename)
nmap <silent>         <leader>lo :<C-u>OrganizeImports<cr>
nmap <silent>         <leader>lz :<C-u>Fold<cr>
nmap <silent>         <leader>lf :<C-u>Format<cr>
nmap                  <leader>lF <Plug>(coc-format-selected)
xmap                  <leader>lf <Plug>(coc-format-selected)
nmap <silent>         <leader>lh :<C-u>call CocAction('doHover')<cr>
" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap                  <leader>lc  <Plug>(coc-codeaction-selected)
nmap                  <leader>lc  <Plug>(coc-codeaction-selected)
" Remap keys for applying codeAction to the current buffer.
nmap                  <leader>la  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap                  <leader>lq  <Plug>(coc-fix-current)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" yank
inoremap <silent> <M-y> <C-O>:<C-U>CocFzfList yank<CR>
nnoremap <silent> <M-y> :<C-U>CocFzfList yank<CR>
xnoremap <silent> <M-y> :<C-U>CocFzfList yank<CR>

" Map function and class text objects
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

command! -nargs=0 Format :call CocAction('format')
command! -nargs=? Fold :call CocAction('fold', <f-args>)
command! -nargs=0 OrganizeImports :call CocAction('runCommand', 'editor.action.organizeImport')

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

augroup mylspgroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " switch alternative file in clangd
  autocmd FileType c nmap <silent> <leader>ls :<C-u>CocCommand clangd.switchSourceHeader<CR>
  autocmd FileType c let g:leader_map.l.s = 'switch-alternative'
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

augroup myrust
  au!
  au FileType rust nmap <silent> <localleader>S :<C-U>CocCommand rust-analyzer.analyzerStatus<CR>
  au FileType rust nmap <silent> <localleader>R :<C-U>CocCommand rust-analyzer.reload<CR>
  au FileType rust nmap <silent> <localleader>W :<C-U>CocCommand rust-analyzer.reloadWorkspace<CR>
  au FileType rust nmap <silent> <localleader>M :<C-U>CocCommand rust-analyzer.expandMacro<CR>
augroup end
" latex {{{ "
augroup mylatex
  autocmd!
  au FileType tex nmap <silent> <C-C><C-V> :<C-U>CocCommand latex.ForwardSearch<CR>
  au FileType tex nmap <silent> <C-C><C-C> :<C-U>CocCommand latex.Build<CR>
  au FileType tex let b:coc_pairs = [["$", "$"]]
augroup end
" }}} latex "
" pairs {{{ "
autocmd FileType markdown let b:coc_pairs_disabled = ['`', '<']
autocmd FileType tex let b:coc_pairs_disabled = ['`', '<', '[']
" }}} pairs "

" }}}

" KEY BINDINGS {{{
" buffers {{{
nnoremap <silent> <leader>bb :<C-u>Buffers<CR>
nnoremap <silent> <leader>bd :<C-u>bd<CR>
nnoremap <silent> <leader>bn :<C-u>bn<CR>
nnoremap <silent> <leader>bp :<C-u>bp<CR>
" }}} buffers
" edit {{{ "
" snippets
nnoremap <silent> <leader>&& :<C-U>Snippets<CR>
nnoremap <silent> <leader>&e :<C-U>UltiSnipsEdit<CR>
nnoremap <silent> <leader>et :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>

" duplicate lines / regions
xmap <M-S-Down> <Plug>(textmanip-duplicate-down)
nmap <M-S-Down> <Plug>(textmanip-duplicate-down)
xmap <M-S-Up> <Plug>(textmanip-duplicate-up)
nmap <M-S-Up> <Plug>(textmanip-duplicate-up)

" move lines / regions
xmap <M-Down> <Plug>(textmanip-move-down)
xmap <M-Up> <Plug>(textmanip-move-up)
xmap <M-Left> <Plug>(textmanip-move-left)
xmap <M-Right> <Plug>(textmanip-move-right)

" insert empty line
nnoremap <silent> [o  :<c-u>put!=repeat([''],v:count)<bar>']+1<cr>
nnoremap <silent> ]o  :<c-u>put =repeat([''],v:count)<bar>'[-1<cr>
" }}} edit "
" emacs binding {{{
noremap <C-g> <Esc>
vnoremap <C-g> <Esc>
cnoremap <C-g> <Esc>
cnoremap <C-BS> <C-W>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-k> <C-\>estrpart(getcmdline(),0,getcmdpos()-1)<CR>
snoremap <C-g> <Esc>
nnoremap <silent> <M-x> :<C-u>Commands<CR>
inoremap <C-g> <Esc>
inoremap <C-e> <C-o>$
inoremap <C-a> <C-o>^
inoremap <C-BS> <C-W>
inoremap <C-k> <C-o>d$
" use C-\ to insert a digraph
inoremap <C-\> <C-k>
" list yanks
inoremap <C-y> <C-o>:<C-u>CocFzfList yanks<CR>
nnoremap <C-y> <C-o>:<C-u>CocFzfList yanks<CR>
" }}} Emacs binding
" files {{{
nnoremap <silent> <leader>ff :<C-U>Files<CR>
nnoremap <silent> <leader>fu :<C-U>History<CR>
" }}} files
" git {{{ "
nnoremap <silent> <leader>gg :<C-U>Gstatus<CR>
" }}} git "
" global {{{

" TODO: it would be better to use rooter
function! s:grep_files()
  let git_dir = system('git rev-parse --show-toplevel 2> /dev/null')[:-2]
  if git_dir != ''
    execute 'Rg' git_dir
  else
    execute 'Rg' '.'
  endif
endfunction

command! MyGrepFiles execute s:grep_files()

nnoremap <silent> <leader>/ :<C-U>MyGrepFiles<CR>
" TODO: it would be better to use rooter
nnoremap <silent> <leader><space> :<C-U>execute system('git rev-parse --is-inside-work-tree') =~ 'true' ? 'GFiles' : 'Files'<CR>
" }}} global
" navigation {{{
" this is an alternative to C-i/C-o because nvim currently cannot map
" tab and C-i to different keys
nnoremap <silent> <M-Left> :<C-U>execute "normal 1\<C-I>"<CR>
nnoremap <silent> <M-Right> :<C-U>execute "normal 1\<C-O>"<CR>

nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
xnoremap <expr> j v:count ? 'j' : 'gj'
xnoremap <expr> k v:count ? 'k' : 'gk'

noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
" }}} navigation
" open {{{
nnoremap <silent> <leader>oq :copen<CR>
nnoremap <silent> <leader>ol :lopen<CR>
nnoremap <silent> <leader>o- :Explore<CR>
" }}} open
" windows {{{
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <C-w><C-w> <C-w>w
nnoremap <leader>w- <C-w>s
nnoremap <leader>w<bar> <C-w>v
nnoremap <leader>w= <C-w>=
nnoremap <leader>wd <C-w>c
nnoremap <leader>ww :<C-u>Windows<CR>
" }}} windows
" search {{{
nnoremap <silent> <leader>ss :<C-U>MyGrepFiles<CR>
nnoremap <silent> <leader>s/ :<C-U>History/<CR>
nnoremap <silent> <leader>sl :<C-u>BLines<CR>
nnoremap <silent> <leader>sL :<C-u>Lines<CR>
nnoremap <silent> <leader>sm :<C-u>Marks<CR>
nnoremap <silent> <leader>st :<C-u>BTags<CR>
nnoremap <silent> <leader>sT :<C-u>Tags<CR>
" }}} search
" toggle {{{
nnoremap <silent> <leader>tg :<C-u>GitGutterToggle<CR>
nnoremap <silent> <leader>th :<C-u>set hls!<CR>
nnoremap <silent> <leader>tp :<C-u>setlocal paste!<CR>
nnoremap <silent> <leader>ts :<C-u>setlocal spell!<CR>
nnoremap <silent> <leader>tw :<C-u>setlocal wrap!<CR>
" }}} toggle "
" vim {{{
nnoremap <silent> <leader>ve :<C-u>edit ~/.config/nvim/init.vim<CR>
nnoremap <silent> <leader>vs :<C-u>update!<CR>:<C-u>source ~/.config/nvim/init.vim<CR>:<C-u>echo "Sourced!"<CR>
nnoremap <silent> <leader>vq :<C-u>quitall<CR>
" }}} vim
" }}}

" plugin: autosave {{{
let g:auto_save = 1
let g:auto_save_write_all_buffers = 1
" }}}

" plugin: easy-motion {{{
" Disable default mappings
let g:EasyMotion_do_mapping = 0
" Turn on case-insensitive feature
let g:EasyMotion_smartcase = 1

nmap gF <Plug>(easymotion-overwin-f2)
nmap gT <Plug>(easymotion-overwin-t2)
nmap gf <Plug>(easymotion-overwin-f)
nmap gt <Plug>(easymotion-overwin-t)

" line motions: Line motions
map gl <Plug>(easymotion-lineforward)
map gj <Plug>(easymotion-j)
map gk <Plug>(easymotion-k)
map gh <Plug>(easymotion-linebackward)
" }}} easy-motion

" easy-align {{{ "
xmap <leader>ea <Plug>(EasyAlign)
nmap <leader>ea <Plug>(EasyAlign)
" }}} easy-align "

" plugin: fugitive {{{ "
augroup mygit
  au!
  au FileType fugitive nmap <TAB> =
augroup end
" }}} plugin: fugitive "

" plugin: fzf {{{

" fzf-spell {{{ "
function! FzfSpellSink(word)
  exe 'normal! "_ciw'.a:word
endfunction

function! FzfSpell()
  let suggestions = spellsuggest(expand("<cword>"))
  return fzf#run({'source': suggestions, 'sink': function("FzfSpellSink"), 'down': 10 })
endfunction

nnoremap z= :call FzfSpell()<CR>
" }}} fzf-spell "

" change the command to pass the --exact
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   "rg --column --line-number --no-heading --color=always --smart-case -- ".shellescape(<q-args>), 1,
  \   fzf#vim#with_preview({ 'options': '--exact'}), <bang>0)

function! s:rg_with_fzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

command! -nargs=* -bang RG call s:rg_with_fzf(<q-args>, <bang>0)

" style preview window
let g:fzf_preview_window = ['up:50%', '?']
" layout
let g:fzf_layout = { 'window': { 'width': 1, 'height': 0.7, 'yoffset': 1, 'xoffset': 0 } }
" which keys will execute the command
let g:fzf_commands_expect = 'alt-enter,ctrl-x'
" copy settings into coc-fzf
let g:coc_fzf_preview = g:fzf_preview_window[0]
let g:coc_fzf_preview_toggle_key = g:fzf_preview_window[1]
let g:coc_fzf_opts = []

" insert mode completion
imap <c-x><s-k> <plug>(fzf-complete-word)
imap <c-x><s-f> <plug>(fzf-complete-path)
imap <c-x><s-l> <plug>(fzf-complete-line)
" }}} fzf

" plugin: markdown {{{
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_auto_insert_bullets = 0
let g:vim_markdown_new_list_item_indent = 0
" }}}

" plugin: rooter {{{
let g:rooter_change_directory_for_non_project_files = 'current'
" }}} rooter

" plugin: ultisnips {{{

let g:UltiSnipsEditSplit = 'context'
let g:UltiSnipsEnableSnipMate = 0
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsSnippetDirectories = ['UltiSnips', 'my-snippets']

" }}} ultisnips

" plugin: sandwich {{{
" prevent s jumping in
nmap s <Nop>
xmap s <Nop>
" }}}

" plugin: slime {{{ "
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": ":.2"}
" }}} plugin: slime "

" plugin: vimtex {{{ "
let g:vimtex_quickfix_open_on_warning = 0
let g:vimtex_view_general_viewer = 'evince'
" }}} plugin: vimtex "

" plugin: which-key {{{

" config {{{
" hide status line when entering which key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
\| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
" disable spell in which key
autocmd FileType which_key setlocal nospell

nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
vnoremap <silent> <leader>      :<c-u>WhichKeyVisual '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
vnoremap <silent> <localleader> :<c-u>WhichKeyVisual ','<CR>

" exit which key using C-G
let g:which_key_exit = "\<C-G>"
" it looks better when it is in a separate window
let g:which_key_use_floating_win = 0
" set directory for key mapping
let g:leader_map =  {}
let g:localleader_map =  {}

aug which_key
  au!
  au User vim-which-key call which_key#register('<Space>', 'g:leader_map')
  au User vim-which-key call which_key#register(',', 'g:localleader_map')
aug END

let g:leader_map['<space>'] = 'open-files'
" }}}

" edit {{{ "
let g:leader_map.e = {
  \ 'name' : '+edit',
  \ 't' : 'delete-trailing-space',
  \ }
" }}} edit "

" snippets {{{ "
let g:leader_map['&'] = {
  \ 'name' : 'snippets',
  \ '&' : 'snippets',
  \ 'e' : 'edit',
  \ }
" }}} snippets "

" +vim {{{
let g:leader_map.v = {
  \ 'name' : '+vim',
  \ 'e' : 'edit-vimrc',
  \ 's' : 'source-vimrc',
  \ 'q' : 'quit-all',
  \ }
" }}}

" +git {{{
let g:leader_map.g = {
  \ 'name' : '+git',
  \ 's' : 'status',
  \ }
let g:leader_map.g.h = { 'name' : '+hunk' }
" }}}

" +files {{{
let g:leader_map.f = { 'name' : '+files' }
" }}}

" +lsp {{{
let g:leader_map.l = {
  \ 'name' : '+lsp',
  \ 'D' : 'diagnostics',
  \ 'E' : 'extensions',
  \ 'C' : 'commands',
  \ 'O' : 'outline',
  \ 'S' : 'symbols',
  \ 'j' : 'next',
  \ 'k' : 'previous',
  \ '''': 'last-list',
  \ 'd' : 'definition',
  \ 't' : 'type-definition',
  \ 'i' : 'implementation',
  \ 'r' : 'references',
  \ 'R' : 'rename',
  \ 'o' : 'organize-imports',
  \ 'z' : 'fold',
  \ 'F' : 'format-selected',
  \ 'f' : 'format',
  \ 'h' : 'hover',
  \ 'a' : 'action',
  \ 'c' : 'action-selected',
  \ 'q' : 'quick-fix',
  \ }
" }}}

" +open {{{
let g:leader_map.o = {
  \ 'name' : '+open',
  \ 'q' : 'open-quickfix'    ,
  \ 'l' : 'open-locationlist',
  \ }
" }}}}

" +buffers {{{
let g:leader_map.b = {
  \ 'name' : '+buffers',
  \ 'b' : 'buffers',
  \ 'd' : 'delete',
  \ 'n' : 'next-buffer',
  \ 'p' : 'previous-buffer',
  \ }
" }}}

" +windows {{{
let g:leader_map.w = {
  \ 'name' : '+windows',
  \ 'w' : 'fzf-windows',
  \ 'd' : ['<C-W>c',     'delete-window'],
  \ '-' : ['<C-W>s',     'split-window-below'],
  \ '|' : ['<C-W>v',     'split-window-right'],
  \ '=' : ['<C-W>=',     'balance-window'],
  \ }
" }}}

" +toggles {{{
let g:leader_map.t = {
  \ 'name' : '+toggle',
  \ 'g' : 'git-status-indicator',
  \ 'h' : 'highlight-search',
  \ 'p' : 'paste-mode',
  \ 's' : 'spell',
  \ 'w' : 'wrap',
  \ '(' : 'rainbow-parens',
  \ }
" }}}

" +notes {{{
let g:leader_map.n = {
  \ 'name' : '+notes',
  \ 'i' : 'index',
  \ 'j' : 'journal',
  \ 'n' : 'notes',
  \ 's' : 'search',
  \ 't' : 'tags',
  \ }
" }}}

" search {{{ "
let g:leader_map.s = {
  \ 'name' : '+search',
  \ 's' : 'search-files',
  \ '/' : 'history',
  \ 'l' : 'buffer-lines',
  \ 'L' : 'all-lines',
  \ 'm' : 'marks',
  \ 't' : 'buffer-tags',
  \ 'T' : 'all-tags',
  \ }
" }}} search "

" }}}

" plugin: wiki {{{
let g:wiki_root = '~/Notes'
let g:wiki_filetypes = ['md']
let g:wiki_link_extension = '.md'
let g:wiki_link_target_type = 'md'
let g:wiki_journal = {
  \ 'name': 'journal',
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
  \ 'output': 'exported',
  \}

" execute 'autocmd BufNewFile ' . g:wiki_root . '/journal/[0-9]*.md :read ' . g:wiki_root . '/../templates/weekly.md'

nmap <silent> <leader>ni :<C-u>WikiIndex<CR>
nmap <silent> <leader>nj :<C-u>WikiJournal<CR>
nmap <silent> <leader>nn :<C-u>WikiFzfPages<CR>
nmap <silent> <leader>nt :<C-u>WikiFzfTags<CR>

let g:wiki_mappings_local = {
  \ '<plug>(wiki-page-delete)':          '<localleader>d',
  \ '<plug>(wiki-list-toggle)':          '<localleader>l',
  \ 'x_<plug>(wiki-link-toggle-visual)': '<localleader><cr>',
  \ '<plug>(wiki-link-next)':            '<localleader>n',
  \ '<plug>(wiki-link-prev)':            '<localleader>p',
  \ '<plug>(wiki-link-return)':          '<localleader>b',
  \ '<plug>(wiki-link-open)':            '<localleader>o',
  \ '<plug>(wiki-export)':               '<localleader>e',
  \ 'x_<plug>(wiki-export)':             '<localleader>e',
  \ '<plug>(wiki-fzf-toc)':              '<localleader>,',
  \ '<plug>(wiki-page-toc)':             '<localleader>t',
  \ }

" }}}

"""
" init.nvim
"""
au FileType vim setlocal foldmethod=marker