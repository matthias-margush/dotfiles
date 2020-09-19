let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

autocmd VimEnter * call asyncomplete#register_source(asyncomplete#sources#ultisnips#get_source_options({
		\ 'name': 'ultisnips',
		\ 'whitelist': ['*'],
		\ 'completor': function('asyncomplete#sources#ultisnips#completor'),
		\ }))

" let g:UltiSnipsListSnippets='<tab>'

" function! s:getAllSnippets()
"   call UltiSnips#SnippetsInCurrentScope(1)
"   " let list = []
" 	let s:active_snips={}
"   for [key, info] in items(g:current_ulti_dict_info)
" 		let s:active_snips[key . ': ' .info.description] = info
"     " let parts = split(info.location, ':')
"     " call add(list, {
"     "   \"key": key,
"     "   \"path": parts[0],
"     "   \"linenr": parts[1],
"     "   \"description": info.description,
"     "   \})
"   endfor
"   return keys(s:active_snips)
" endfunction

" let g:clap_provider_snippets = {
" 			\ 'source': function('s:getAllSnippets'),
" 			\ 'sink': 'echo'
" 			\ }

