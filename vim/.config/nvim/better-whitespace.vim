let g:better_whitespace_enabled = 0 " Disables highlighting
let g:strip_whitespace_on_save = 1
let g:strip_whitespace_confirm = -1
let g:better_whitespace_operator=''
let g:strip_only_modified_lines=1
autocmd FileType markdown,go DisableStripWhitespaceOnSave
