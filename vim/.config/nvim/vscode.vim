
call plug#begin('~/.local/share/nvim/plugged')
  Plug 'tommcdo/vim-lion'               | let g:lion_squeeze_spaces = 1
  Plug 'romainl/vim-cool' 
call plug#end()

xmap gc  <Plug>VSCodeCommentary
nmap gc  <Plug>VSCodeCommentary
omap gc  <Plug>VSCodeCommentary
nmap gcc <Plug>VSCodeCommentaryLine

nmap <silent> ,, :set opfunc=<SID>send_to_term<CR>g@
vmap <silent> ,, :<C-U>call <SID>send_to_term(visualmode(), 1)<CR>

function! s:send_to_term(type, ...)
let sel_save = &selection
let &selection = "inclusive"
let reg_save = @@

if a:0  " Invoked from Visual mode, use '< and '> marks.
  silent exe "normal! `<" . a:type . "`>y"
elseif a:type == 'line'
  silent exe "normal! '[V']y"
elseif a:type == 'block'
  silent exe "normal! `[\<C-V>`]y"
else
  silent exe "normal! `[v`]y"
endif

let selection = @@
if trim(selection) != ""
  call VSCodeNotify(
        \ "workbench.action.terminal.sendSequence",
        \ {'text': "\e[200~".selection."\e[201~\n"}
        \ )
endif

let &selection = sel_save
let @@ = reg_save
endfunction