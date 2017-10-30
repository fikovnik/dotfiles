set termguicolors

" start without the toolbar
set guioptions-=T

if has("gui_macvim")
  " Fullscreen takes up entire screen
  set fuoptions=maxhorz,maxvert

  set lines=55				" number of rows
  set columns=150				" number of columns
  set anti 				    " auto aliasing
  set showtabline=1			" show tabline only when there is at least two files
  set numberwidth=5
  set cursorline
  set guioptions=emr

  " default font
  set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ Nerd\ Font\ Complete\ Mono:h13

  " custom highligh
  "hi Normal                    guibg=#323232
  "hi CursorLine                guibg=#333435
  "hi Search                    guibg=#5A647E
  "hi Visual                    guibg=#5A647E
  "hi LineNr                    guibg=#e3e3e3 guifg=#949494 gui=NONE
  "hi StatusLine                guibg=#414243 guifg=#e3e3e3 gui=NONE
  "hi StatusLineNC              guibg=#414243 guifg=#949494 gui=NONE
  "hi VertSplit                 guibg=#414243 gui=NONE
  "hi NonText                   guifg=#525252
  "hi MatchParen                guifg=#FFFFFF guibg=#F200FF gui=bold
  "hi Folded                    guifg=#ffffff guibg=#F77A22 gui=NONE
  "hi SpellBad                  guifg=NONE    guibg=NONE    gui=undercurl
  "hi SpellCap                  guifg=NONE    guibg=NONE    gui=undercurl

  " Set up the gui cursor to look nice
  set guicursor=n-v-c:block-Cursor-blinkon0
  set guicursor+=ve:ver35-Cursor
  set guicursor+=o:hor50-Cursor
  set guicursor+=i-ci:ver25-Cursor
  set guicursor+=r-cr:hor20-Cursor
  set guicursor+=sm:block-Cursor-blinkwait175-blinkoff150-blinkon175
endif

" bibble single line / selection up / down
nmap <M-Up> [e
vmap <M-Up> [egv
nmap <M-Down> ]e
vmap <M-Down> ]egv
