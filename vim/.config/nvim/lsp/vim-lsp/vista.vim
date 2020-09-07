nnoremap <silent> <leader>vv :Vista!!<cr>
nnoremap <silent> <leader>vf :Vista finder fzf<cr>
let g:vista_stay_on_open=1
let g:vista_disable_statusline=1
let g:vista_default_executive='vim_lsp'
let g:vista_executive_for = {
			\ 'go': 'vim_lsp',
			\}

let g:vista_icon_indent=['  ', '  ']
let g:vista_highlight_whole_line=1
