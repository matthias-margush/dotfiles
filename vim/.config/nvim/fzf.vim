let g:fzf_history_dir = '~/.local/share/fzf-history' " history

function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
endfunction

let g:fzf_action = {
      \ 'ctrl-q': function('s:build_quickfix_list'),
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-x': 'split',
      \ 'ctrl-v': 'vsplit' }

nnoremap <silent> <leader>pf :Files<cr>

nnoremap <silent> <leader><leader> :Buffers<cr>
nnoremap <silent> <leader>mm :Maps<cr>
nnoremap <silent> <leader>/ :Rg<cr>
" nnoremap <silent> <leader>gc :Commits<cr>
" nnoremap <silent> <leader>gb :BCommits<cr>
nnoremap <silent> <leader>: :History:<cr>
" nnoremap <silent> <A-;> :Commands<cr>
" nnoremap <silent> <leader>s :Snippets<cr>

command! -bang -nargs=* GGrep
      \ call fzf#vim#grep(
      \   'git grep --line-number '.shellescape(<q-args>), 0,
      \   { 'dir': systemlist('git rev-parse --show-toplevel')[0] }, <bang>0)

let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6 } }

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(<q-args>), 0,
  \   fzf#vim#with_preview('up'), <bang>0)

command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(
      \   <q-args>,
      \   fzf#vim#with_preview({'window': {'width': 0.9, 'height': 0.6}}, 'up'),
      \   <bang>0)

command! -bang -nargs=? -complete=dir Buffers
       \ call fzf#vim#buffers(
       \   <q-args>,
       \   fzf#vim#with_preview({'window': {'width': 0.9, 'height': 0.6}}, 'up'),
       \ <bang>0)

highlight link fzf1 Normal
highlight link fzf2 Normal
highlight link fzf3 Normal

if has('nvim')
  tnoremap <expr> <M-r> '<C-\><C-N>"'.nr2char(getchar()).'pi'
endif
