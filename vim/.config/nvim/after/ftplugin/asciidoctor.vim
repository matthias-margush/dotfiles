let g:asciidoctor_syntax_conceal = 1
" nnoremap <silent><buffer><leader>cc :write<cr>\|:Dispatch! adoc-preview '%'<cr>
autocmd BufWritePost <buffer> :silent! Dispatch! adoc-preview '%'
nnoremap <buffer><leader>np :AsyncRun -mode=term -pos=hide adoc-watch '%'<cr>
" nnoremap <buffer><leader>np :!adoc-watch '%'<cr>
setlocal isfname+=32
setlocal isfname-=+
setlocal shiftwidth=2
setlocal tabstop=2
setlocal expandtab
setlocal nowrap
setlocal linebreak

