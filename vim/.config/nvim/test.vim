let test#strategy='dispatch'
let g:neoterm_default_mod='botright'

" let g:test#runner_commands=['GoTest', 'RichGo']
let g:test#runner_commands=['GoTest', 'gotest']

nmap <silent> <leader>tn :TestNearest<CR>
nmap <silent> <leader>tf :TestFile<CR>
nmap <silent> <leader>ta :TestSuite<CR>
nmap <silent> <leader>tl :TestLast<CR>
nmap <silent> <leader>tg :TestVisit<CR>
nmap <silent> <leader>tp :T go test ./...<CR>
