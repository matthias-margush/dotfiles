" let g:limelight_default_coefficient = 0.94
" let g:limelight_bop = '^---'
" let g:limelight_eop = '^---'
let g:limelight_conceal_ctermfg='gray'
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!
nnoremap <silent> <leader>dd :Goyo<cr>

function! s:auto_goyo()
  if &ft == 'markdown'
    Goyo 80
  elseif exists('#goyo')
    let bufnr = bufnr('%')
    Goyo!
    execute 'b '.bufnr
  endif
endfunction

augroup goyo_markdown
  autocmd!
  autocmd BufNewFile,BufRead * call s:auto_goyo()
	autocmd VimLeave * :silent Goyo!
augroup END

" autocmd VimEnter,BufEnter,BufNewFile,BufRead * call s:auto_goyo()
" autocmd BufRead,BufNewFile * :Goyo 80

" function s:present()
" 	if !exists("b:presentation_running")
" 		let b:presentation_running=0
" 	endif

" 	if b:presentation_running
" 		let &l:guicursor = b:saved_guicursor
" 		Goyo!
" 	else
" 		Goyo 70%
" 		let b:saved_guicursor = &guicursor
" 	endif
" endfunction

" function! s:goyo_enter()
" 	let b:presentation_running=1
" 	silent !tmux set status off
" 	set noshowmode
" 	set noshowcmd
" 	set nohlsearch
" 	setlocal guicursor=n-v-c:hor20
" 	let g:vim_markdown_concealing=0
" 	nnoremap <buffer><silent> ]] /----*<cr>zt
" 	nnoremap <buffer><silent> [[ ?----*<cr>zt
" 	let &scrolloff = (&lines / 6)
" 	set report=99999
" 	" set nowrapscan
" 	Limelight
" endfunction

" function! s:goyo_leave()
" 	let b:presentation_running=0
" 	silent !tmux set status on
" 	set showmode
" 	set showcmd
" 	set scrolloff=0
" 	set report=2
" 	set wrapscan
" 	setlocal guicursor=n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20
" 	let g:vim_markdown_concealing=0
" 	Limelight!
" endfunction

" autocmd! User GoyoEnter nested call <SID>goyo_enter()
" autocmd! User GoyoLeave nested call <SID>goyo_leave()
" autocmd! VimLeave * :silent !tmux set status on

" command! Present call s:present()
" " nnoremap <silent> <leader>p :Present<cr>
