" Go:
" cd to a directory outside of GOPATH or a module, then:
"
"     GO111MODULE=on go get golang.org/x/tools/gopls@latest
"

lua require'nvim_lsp_config'

sign define LspDiagnosticsErrorSign text=✗ texthl=LspDiagnosticsError linehl= numhl=
sign define LspDiagnosticsWarningSign text=✗ texthl=LspDiagnosticsWarning linehl= numhl=
sign define LspDiagnosticsInformationSign text=✗ texthl=LspDiagnosticsInformation linehl= numhl=
sign define LspDiagnosticsHintSign text=✗ texthl=LspDiagnosticsHint linehl= numhl=

" autocmd BufEnter * lua require'completion'.on_attach()
" let g:completion_enable_snippet = 'UltiSnips'
let g:completion_timer_cycle = 700
let g:completion_enable_auto_popup = 1
let g:diagnostic_auto_popup_while_jump = 0

function! s:ConfigureBuffer()
	echo "Configuring LSP"
  let &omnifunc="v:lua.vim.lsp.omnifunc"
  " nnoremap <buffer><silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
  nnoremap <buffer><silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>
  nnoremap <buffer><silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
  nnoremap <buffer><silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
  nnoremap <buffer><silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
  nnoremap <buffer><silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
  nnoremap <buffer><silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
  nnoremap <buffer><silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
  nnoremap <buffer><silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
  nnoremap <buffer><silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
  nnoremap <buffer><silent> ]g    :NextDiagnostic<CR>
  nnoremap <buffer><silent> [g    :PrevDiagnostic<CR>
  nnoremap <buffer><silent> <Leader>df <cmd>lua vim.lsp.buf.formatting()<CR>
	nnoremap <buffer><silent> <leader>q <cmd>lua vim.lsp.buf.code_action()<CR>

  " if exists('+signcolumn')
  "   setlocal signcolumn=yes
  " endif
endfunction

autocmd FileType go call s:ConfigureBuffer()
autocmd FileType clojure call s:ConfigureBuffer()
