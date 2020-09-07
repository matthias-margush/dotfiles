let g:lsp_diagnostics_echo_cursor = 1
let g:lsp_diagnostics_enabled = 1
let g:lsp_diagnostics_float_cursor = 1
let g:lsp_highlights_enabled = 1
let g:lsp_highlight_references_enabled = 0
let g:lsp_semantic_enabled = 1
let g:lsp_insert_text_enabled = 0
let g:lsp_preview_float = 0
let g:lsp_signature_help_enabled = 0
let g:lsp_signs_enabled = 1
let g:lsp_virtual_text_enabled = 0
let g:lsp_signs_error =  {'text': '✗'}
let g:lsp_signs_warning = {'text': '✗'}
let g:lsp_signs_hint = {'text': '✗'}
let g:lsp_preview_doubletap=0
let g:lsp_ultisnips_integration=1
let g:lsp_documentation_float=0

function! ToggleHighlightReferences()
  if exists("g:lsp_highlight_references_enabled") && g:lsp_highlight_references_enabled == 0
    let g:lsp_highlight_references_enabled=1
  else
    let g:lsp_highlight_references_enabled=0
  endif
endfunction

function! MyFormatDoc() abort
  try
    silent LspDocumentFormatSync
  catch
  endtry
  try
    silent LspCodeActionSync source.organizeimports
  catch
  endtry
endfunction

function! MyFormatDocAsync() abort
  try
    silent LspDocumentFormat
  catch
  endtry
  try
    silent LspCodeAction source.organizeimports
  catch
  endtry
endfunction

function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete

  " setlocal foldmethod=expr
	" \ foldexpr=lsp#ui#vim#folding#foldexpr()

  nmap <silent> <buffer> K <plug>(lsp-hover)
  nmap <silent> <buffer> gd <plug>(lsp-definition)
  nmap <silent> <buffer> gr <plug>(lsp-references)
  nmap <silent> <buffer> <leader>q <plug>(lsp-code-action)
  nmap <silent> <buffer> <leader>r <plug>(lsp-rename)
  nmap <silent> <buffer> <leader>j <plug>(lsp-document-symbol)
  nmap <silent> <buffer> ]g <plug>(lsp-next-diagnostic)
  nmap <silent> <buffer> [g <plug>(lsp-previous-diagnostic)
  nmap <silent> <buffer> gqq <plug>(lsp-document-format)
  vmap <silent> <buffer> gq <plug>(lsp-document-format)
  xmap <silent> <buffer> gq <plug>(lsp-document-range-format)
  nmap <silent> <buffer> <leader>dd :LspDocumentDiagnostics<cr>
  nmap <silent> <buffer> <leader>a :LspCodeAction<cr>
  nnoremap <silent><buffer> <leader>tr :call ToggleHighlightReferences()<cr>

  nnoremap <silent><buffer> <leader>df :LspDocumentFormat<cr>
  " autocmd BufWritePre <buffer> call s:format_doc()
endfunction

augroup lsp_install
  autocmd!
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

" let g:lsp_settings = {
"       \ 'gopls': {'cmd': ['gopls', '-remote=auto'],}
"       \ }
