nnoremap <silent> <leader>v :Vista!!<cr>
let g:vista_stay_on_open=1
let g:vista_disable_statusline=1
let g:vista_default_executive='coc'
let g:vista_executive_for = {
			\ 'go': 'coc',
			\}
function! NearestMethodOrFunction() abort
  return get(b:, 'vista_nearest_method_or_function', '')
endfunction

function! EchoMethodOrFunction() abort
  echo NearestMethodOrFunction()
endfunction

set statusline+=%{NearestMethodOrFunction()}

nnoremap <silent> <Leader>tf :call EchoMethodOrFunction()<cr>

set statusline+=%{NearestMethodOrFunction()}

" By default vista.vim never run if you don't call it explicitly.
"
" If you want to show the nearest function in your statusline automatically,
" you can add the following line to your vimrc
" autocmd VimEnter * call vista#RunForNearestMethodOrFunction()
