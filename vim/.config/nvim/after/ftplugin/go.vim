" let g:go_fmt_command = "goimports"
" let g:go_gopls_options = ['-remote=auto']
" let g:go_info_mode='gopls'
" let g:go_jump_to_error = 0
" let g:go_referrers_mode = 'gopls'
let g:go_code_completion_enabled = 0
let g:go_def_mapping_enabled = 0
let g:go_doc_keywordprg_enabled = 0
let g:go_fmt_autosave = 0
let g:go_gopls_enabled = 0
let g:go_mod_fmt_autosave = 0
let g:go_test_show_name = 1
setlocal path+=**

let test#go#runner='gotest'

nnoremap <silent><buffer> <leader>cl :Makery lint<cr>
nnoremap <silent><buffer> <leader>cc :Makery make<cr>

compiler gotest
