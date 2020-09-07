" From :help setting-tabline
function! MyTabLabel(n)
	let buflist = tabpagebuflist(a:n)
	let winnr = tabpagewinnr(a:n)
	let bufname = bufname(buflist[winnr - 1])
	if bufname =~ "NERD"
		return '————————'
	endif
	return fnamemodify(bufname, ':p:t:~:.')
endfunction

function! MyTabLine()
	let s = ''
	let p = 0
	let i=0
	let n=tabpagenr('$')
	if n == 1 && strlen(MyTabLabel(1)) == 0
		let s .= repeat(' 𐬹', &columns / 2)
		return s
	endif

	while i < n
		let i += 1
		if i == 1
			let p += 2
			let s .= '%#TabLineBetween# '
		endif
		if i == tabpagenr()
			let s .= '%#TabLineSel#'
		else
			let s .= '%#TabLine#'
		endif

		" set the tab page number (for mouse clicks)
		let s .= '%' . (i) . 'T'

		" the label is made by MyTabLabel()
		let s .= '%{MyTabLabel(' . (i) . ')}%#TabLineBetween#'
		let p += strlen(MyTabLabel(i))
		if i < n
			" let s .= ' ⳼ '
			let s .= ' 𐬹 '
			let p += 3
		endif
		if i >= n
			let s .= ' %#TabFill#'
			let p += 2
		endif
	endwhile

	if p > 0
		let gap=5
		let left = (&columns - p) / 2 / gap
		let s = repeat('     ', left) . s
		let right = (&columns - p - (left * gap)) / gap
		let s .=  repeat('     ', right)
		let extra = &columns - (left * gap) - (right * gap) - p
		let s = repeat(' ', extra - (extra / 2)) . s
	endif

	return s
endfunction

set tabline=%!MyTabLine()

set showtabline=1
let g:tablineclosebutton=0
