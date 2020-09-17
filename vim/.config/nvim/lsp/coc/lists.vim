nnoremap <silent> <leader>ll       :<C-u>CocList<cr>
nnoremap <silent> <leader>lr       :<C-u>CocListResume<cr>
nnoremap <silent> <leader>c        :<C-u>CocList commands<cr>
nnoremap <silent> <leader>a        :<C-u>CocList actions<cr>
nnoremap <silent> <leader>pf       :<C-u>CocList files<cr>
nnoremap <silent> <leader><leader> :<C-u>CocList buffers<cr>
nnoremap <silent> <leader>k        :<C-u>CocList --ignore-case maps<cr>
nnoremap <silent> <leader>m        :<C-u>CocList marks<cr>
nnoremap <silent> <leader>/        :<C-u>CocList --interactive grep<cr>
nnoremap <silent> <leader>j        :<C-u>CocList outline<cr>
nnoremap <silent> <leader>y        :<C-u>CocList --normal yank<cr>

highlight link CocListsDesc ErrorSign
