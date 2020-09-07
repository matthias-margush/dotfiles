augroup ParinferBuffer
	autocmd! * <buffer>
	autocmd InsertEnter <buffer> ToggleParinferMode
	autocmd InsertLeave <buffer> ToggleParinferMode
augroup END
" nnoremap <silent><buffer> <lt><lt> :ToggleParinferMode<cr><lt><lt>:ToggleParinferMode<cr>
" nnoremap <silent><buffer> >> :ToggleParinferMode<cr>>>:ToggleParinferMode<cr>
" nnoremap <silent><buffer> dd :ToggleParinferMode<cr>dd:ToggleParinferMode<cr>
" nnoremap <silent><buffer> D :ToggleParinferMode<cr>D:ToggleParinferMode<cr>
" inoremap <silent><buffer> <c-d> <c-o>:ToggleParinferMode<cr><c-d><c-o>:ToggleParinferMode<cr>
" inoremap <silent><buffer> <c-t> <c-o>:ToggleParinferMode<cr><c-t><c-o>:ToggleParinferMode<cr>
" inoremap <silent><buffer> <tab> <c-o>:ToggleParinferMode<cr><tab><c-o>:ToggleParinferMode<cr>

"" Iced keys
silent! nmap <buffer> <LocalLeader><LocalLeader> <Plug>(iced_connect)

"" Evaluating (<LocalLeader>e)
"" ------------------------------------------------------------------------
silent! nmap <buffer> <LocalLeader>eq <Plug>(iced_interrupt)
silent! nmap <buffer> <LocalLeader>eQ <Plug>(iced_interrupt_all)
silent! nmap <buffer> <LocalLeader>" <Plug>(iced_jack_in)
silent! nmap <buffer> <LocalLeader>ei <Plug>(iced_eval)<Plug>(sexp_inner_element)``
silent! nmap <buffer> <LocalLeader>ee <Plug>(iced_eval)<Plug>(sexp_outer_list)``
silent! nmap <buffer> <LocalLeader>et <Plug>(iced_eval_outer_top_list)
silent! nmap <buffer> <LocalLeader>ea <Plug>(iced_eval_at_mark)
silent! nmap <buffer> <LocalLeader>el <Plug>(iced_eval_last_outer_top_list)
silent! vmap <buffer> <LocalLeader>ee <Plug>(iced_eval_visual)
silent! nmap <buffer> <LocalLeader>en <Plug>(iced_eval_ns)
silent! nmap <buffer> <LocalLeader>ep <Plug>(iced_print_last)
silent! nmap <buffer> <LocalLeader>eb <Plug>(iced_require)
silent! nmap <buffer> <LocalLeader>eB <Plug>(iced_require_all)
silent! nmap <buffer> <LocalLeader>eu <Plug>(iced_undef)
silent! nmap <buffer> <LocalLeader>eU <Plug>(iced_undef_all_in_ns)
silent! nmap <buffer> <LocalLeader>eM <Plug>(iced_macroexpand_outer_list)
silent! nmap <buffer> <LocalLeader>em <Plug>(iced_macroexpand_1_outer_list)

"" Testing (<LocalLeader>t)
"" ------------------------------------------------------------------------
silent! nmap <buffer> <LocalLeader>tt <Plug>(iced_test_under_cursor)
silent! nmap <buffer> <LocalLeader>tl <Plug>(iced_test_rerun_last)
silent! nmap <buffer> <LocalLeader>ts <Plug>(iced_test_spec_check)
silent! nmap <buffer> <LocalLeader>to <Plug>(iced_test_buffer_open)
silent! nmap <buffer> <LocalLeader>tn <Plug>(iced_test_ns)
silent! nmap <buffer> <LocalLeader>tp <Plug>(iced_test_all)
silent! nmap <buffer> <LocalLeader>tr <Plug>(iced_test_redo)

"" Stdout buffer (<LocalLeader>s)
"" ------------------------------------------------------------------------
silent! nmap <buffer> <LocalLeader>ss <Plug>(iced_stdout_buffer_open)
silent! nmap <buffer> <LocalLeader>sl <Plug>(iced_stdout_buffer_clear)
silent! nmap <buffer> <LocalLeader>sq <Plug>(iced_stdout_buffer_close)

"" Refactoring (<LocalLeader>r)
"" ------------------------------------------------------------------------
silent! nmap <buffer> <LocalLeader>rcn <Plug>(iced_clean_ns)
silent! nmap <buffer> <LocalLeader>rca <Plug>(iced_clean_all)
silent! nmap <buffer> <LocalLeader>ram <Plug>(iced_add_missing)
silent! nmap <buffer> <LocalLeader>ran <Plug>(iced_add_ns)
silent! nmap <buffer> <LocalLeader>rtf <Plug>(iced_thread_first)
silent! nmap <buffer> <LocalLeader>rtl <Plug>(iced_thread_last)
silent! nmap <buffer> <LocalLeader>ref <Plug>(iced_extract_function)
silent! nmap <buffer> <LocalLeader>raa <Plug>(iced_add_arity)
silent! nmap <buffer> <LocalLeader>rml <Plug>(iced_move_to_let)

"" Help/Document (<LocalLeader>h)
"" ------------------------------------------------------------------------
" silent! nmap <buffer> K <Plug>(iced_document_popup_open)
silent! nmap <buffer> <LocalLeader>k <Plug>(iced_document_open)
silent! nmap <buffer> <LocalLeader>hu <Plug>(iced_use_case_open)
silent! nmap <buffer> <LocalLeader>hn <Plug>(iced_next_use_case)
silent! nmap <buffer> <LocalLeader>hN <Plug>(iced_prev_use_case)
silent! nmap <buffer> <LocalLeader>hq <Plug>(iced_document_close)
silent! nmap <buffer> <LocalLeader>hS <Plug>(iced_source_show)
silent! nmap <buffer> <LocalLeader>hs <Plug>(iced_source_popup_show)
silent! nmap <buffer> <LocalLeader>hc <Plug>(iced_clojuredocs_open)
silent! nmap <buffer> <LocalLeader>hh <Plug>(iced_command_palette)

"" Browsing (<LocalLeader>b)
"" ------------------------------------------------------------------------
silent! nmap <buffer> <LocalLeader>bn <Plug>(iced_browse_related_namespace)
silent! nmap <buffer> <LocalLeader>bs <Plug>(iced_browse_spec)
silent! nmap <buffer> <LocalLeader>bt <Plug>(iced_browse_test_under_cursor)
silent! nmap <buffer> <LocalLeader>br <Plug>(iced_browse_references)
silent! nmap <buffer> <LocalLeader>bd <Plug>(iced_browse_dependencies)
silent! nmap <buffer> <LocalLeader>bvr <Plug>(iced_browse_var_references)
silent! nmap <buffer> <LocalLeader>bvd <Plug>(iced_browse_var_dependencies)

"" Jumping cursor (<LocalLeader>j)
"" ------------------------------------------------------------------------
" silent! nmap <buffer> <C-]> <Plug>(iced_def_jump)
silent! nmap <buffer> <LocalLeader>jn <Plug>(iced_jump_to_next_sign)
silent! nmap <buffer> <LocalLeader>jN <Plug>(iced_jump_to_prev_sign)
silent! nmap <buffer> <LocalLeader>jl <Plug>(iced_jump_to_let)

"" Debugging (<LocalLeader>d)
"" ------------------------------------------------------------------------
silent! nmap <buffer> <LocalLeader>dbt <Plug>(iced_browse_tapped)
silent! nmap <buffer> <LocalLeader>dlt <Plug>(iced_clear_tapped)

"" Misc
"" ------------------------------------------------------------------------
silent! nmap <buffer> == <Plug>(iced_format)
silent! nmap <buffer> =G <Plug>(iced_format_all)
" silent! nmap <buffer> <LocalLeader>* <Plug>(iced_grep)
" silent! nmap <buffer> <LocalLeader>/ :<C-u>IcedGrep<Space>
