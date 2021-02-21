inoremap <silent><expr> <c-x><c-o> coc#refresh()

runtime lsp/coc/bookmark.vim
runtime lsp/coc/git.vim
runtime lsp/coc/lists.vim
runtime lsp/coc/snippets.vim

nnoremap <silent> K :call <SID>show_documentation()<CR>
nnoremap <silent> <Leader>d :CocList diagnostics<CR>
nnoremap <silent> <Leader><Leader> :CocList buffers<CR>
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> <leader>u <Plug>(coc-references)
nmap <silent> <leader>r <Plug>(coc-rename)
nmap <silent> <a-g> :echo b:coc_current_function<Cr>

" Introduce function text object
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" nnoremap <leader>tr :silent call CocActionAsync('highlight')<cr>
autocmd CursorHold * silent call CocActionAsync('highlight')

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

set formatexpr=CocAction('formatSelected')

inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Fix autofix problem of current line
nmap <silent> <leader>q <Plug>(coc-fix-current)
xmap <silent> <leader>a <plug>(coc-codeaction-selected)

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

let g:coc_global_extensions = [
	\ 'coc-diagnostic',
	\ 'coc-docker',
	\ 'coc-git',
	\ 'coc-go',
	\ 'coc-highlight',
	\ 'coc-json',
	\ 'coc-lists',
	\ 'coc-marketplace',
	\ 'coc-pairs',
	\ 'coc-sh',
	\ 'coc-sql',
	\ 'coc-snippets',
	\ 'coc-vimlsp',
	\ 'coc-yaml'
	\ ]
  " \ coc-markdownlint
  " \ coc-sql " npm install sql-formatter; npm install node-sql-parser --save
  " coc-yank
