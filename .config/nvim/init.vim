" PLUGINS {{{
call plug#begin()

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
nnoremap <silent> <leader>bd <cmd>bd<CR>
nnoremap <silent> <leader>bn <cmd>bn<CR>
nnoremap <silent> <leader>bp <cmd>bp<CR>
" }}}
" edit {{{
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
inoremap <M-q> <C-o>gwap
nnoremap <M-q> gwap
vnoremap <M-q> gw
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
" }}}
" open {{{
nnoremap <silent> <leader>o- <cmd>Explore<CR>
nnoremap <silent> <leader>ol <cmd>lopen<CR>
nnoremap <silent> <leader>oq <cmd>copen<CR>
" }}}
" terminal {{{
tnoremap jk <C-\><C-n>
" }}}
" toggle {{{
nmap <silent> <leader>th <cmd>set hls!<CR>
nmap <silent> <leader>tp <cmd>setlocal paste!<CR>
nmap <silent> <leader>ts <cmd>setlocal spell!<CR>
nmap <silent> <leader>tw <cmd>setlocal wrap!<CR>
" }}}
" vim {{{
nmap <silent> <leader>ve <cmd>edit ~/.config/nvim/init.vim<CR>
nmap <silent> <leader>vfs <cmd>syntax sync fromstart<CR>
nmap <silent> <leader>vpU <cmd>PlugUpgrade<CR>
nmap <silent> <leader>vpc <cmd>PlugClean<CR>
nmap <silent> <leader>vpi <cmd>PlugInstall<CR>
nmap <silent> <leader>vps <cmd>PlugStatus<CR>
nmap <silent> <leader>vpu <cmd>PlugUpdate<CR>
nmap <silent> <leader>vq <cmd>quitall<CR>
nmap <silent> <leader>vs <cmd>update!<CR>:<C-u>source ~/.config/nvim/init.vim<CR>:<C-u>echo "Sourced!"<CR>
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
nnoremap <leader>wk <C-w>c
nnoremap <leader>wH <C-w>H
nnoremap <leader>wJ <C-w>J
nnoremap <leader>wK <C-w>K
nnoremap <leader>wL <C-w>L
" }}}
" }}}

" file-type: vim {{{
augroup my-vim
  au!
  au FileType vim setlocal foldmethod=marker
augroup end
" }}}
