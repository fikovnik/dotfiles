vim.cmd [[
function! MyVimuxSlime()
 call VimuxRunCommand(@v)
endfunction

function! MyVimuxSlimeMotion(type)
    silent execute 'normal! `[v`]"vy'
    call MyVimuxSlime()
endfunction

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
]]

vim.g.VimuxCloseOnExit = false
vim.g.VimuxUseNearest = true
