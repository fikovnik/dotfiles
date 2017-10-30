runtime bundle/vim-pathogen/autoload/pathogen.vim
execute pathogen#infect()

let mapleader = ","

if $TERM_PROGRAM =~ "iTerm"
  " terminal mapping

  " true colors
  set termguicolors

  " bubble lines / selection up/down
  nmap [1;9A [e   "<M-Up>
  vmap [1;9A [egv "<M-Up>
  nmap [1;9B ]e   "<M-Down>
  vmap [1;9B ]egv "<M-Down>
  " alt+backspace deletes a word - it comes from iTerm2 sending '\<C-W>'
endif

" colors
syntax enable
set background=dark
"colorscheme solarized
colorscheme solarized8_dark

" custom colors
hi SpellBad cterm=underline ctermfg=red

" tabs/indent
set shiftwidth=2
set tabstop=2
set softtabstop=2
set expandtab
set colorcolumn=90

" smart break indent
set breakindent
let &showbreak = '↪ '
set linebreak

set cursorline
set number
set cmdheight=2
set encoding=utf-8
set hidden                                  " allow to work with hidden buffers
set undolevels=1000
set showmatch
set showmode
set noerrorbells
set visualbell
set clipboard=unnamed
set listchars=tab:•\ ,trail:•,extends:»,precedes:« " Unprintable chars mapping
set nofoldenable

" search
set hlsearch
set incsearch
set wrapscan
set ignorecase

set backspace=indent,eol,start
set completeopt=longest,menuone
set complete=.,w,b,kspell,ss
set scrolloff=2

set mouse=a

" cursor change between modes
if exists('$TMUX')
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" automatically reloads RC files
if has ('autocmd') " Remain compatible with earlier versions
  " vimrc files
  augroup vimrc
    " Source vim configuration upon save
    autocmd! BufWritePost $MYVIMRC source % | echom "Reloaded " . $MYVIMRC | redraw
    autocmd! BufWritePost $MYGVIMRC if has('gui_running') | so % | echom "Reloaded " . $MYGVIMRC | endif | redraw
  augroup END

endif

" commenting
nmap <C-_> :call NERDComment(0,"toggle")<CR>
vmap <C-_> :call NERDComment(0,"toggle")<CR>

" moving in the command-line
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-f> <Right>
cnoremap <A-BS> <C-w>

" Toggle wrap
nmap <Leader>w :set wrap! list! linebreak!<CR>

" bubble single line / selection up / down
nmap <M-Up> [e
vmap <M-Up> [egv
nmap <M-Down> ]e
vmap <M-Down> ]egv

" Forgot sudo before editing
"     http://forrst.com/posts/Use_w_to_sudo_write_a_file_with_Vim-uAN
cmap w!! w !sudo tee % >/dev/null

" switching buffers
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [B :bfirst<CR>
nnoremap <silent> ]B :blast<CR>

inoremap <silent> <C-x><C-s> <C-o>:w<CR>
nnoremap <silent> <C-x><C-s> :w<CR>

" plugins
" -----------------------------------

" vim-airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
"let g:airline_theme='papercolor'
let g:airline_theme='solarized'
let g:airline_powerline_fonts=1

" CtrlP
nmap <silent> <C-x><C-f> :CtrlP<CR>
nmap <silent> <C-x><C-b> :CtrlPBuffer<CR>
nmap <silent> <C-x><C-r> :CtrlPMRU<CR>
