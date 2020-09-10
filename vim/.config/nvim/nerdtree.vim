let NERDTreeMinimalUI=1
let NERDTreeHijackNetrw=0
let NERDTreeStatusline=''
let NERDTreeAutoDeleteBuffer=1
let NERDTreeRespectWildIgnore=1
let NERDTreeDirArrowExpandable=" "
let NERDTreeDirArrowCollapsible=" "


function! NerdTreeToggleFind()
    if exists("g:NERDTree") && g:NERDTree.IsOpen()
        NERDTreeClose
    elseif filereadable(expand('%'))
        NERDTreeFind
    else
        NERDTree
    endif
endfunction

nnoremap <silent> <leader>pt :call NerdTreeToggleFind()<cr>

