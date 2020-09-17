let g:loaded_matchparen = 1 " disables flashing matching paren
let g:python_host_prog = '/usr/local/bin/python'
let g:python3_host_prog = '/usr/local/bin/python3'
let g:ruby_host_prog = '/usr/local/opt/ruby/bin/ruby'
set exrc secure
let loaded_matchit = 1
set signcolumn=yes        " sign column visibility
set autoread              " automatically reload files that changed
set formatoptions-=o
set noshowmode

set incsearch
set diffopt+=vertical,internal,algorithm:patience
set completeopt=menu,noinsert,noselect ",preview
" set foldlevelstart=99     " don't fold
set foldlevelstart=99
set foldcolumn=0
set foldmethod=syntax
set foldnestmax=1
set grepprg=ag\ --nogroup\ --nocolor
set hidden                " allow switching from unsaved buffers
set laststatus=1          " show status line if there is more than 1 window
set guioptions-=rL
set conceallevel=0
set nolazyredraw
set modifiable
set mouse=a               " enable mouse in terminal
set nocursorline          " current line highlighting
set nolist
set noruler               " hide line/col status
set previewheight=10
set nospell
set spellfile=.en.utf8.add,~/.config/nvim/spell/en.utf8.add
set noswapfile            " whether to use a swap file
set nowrap                " no line wrapping
" set omnifunc=syntaxcomplete#Complete
set path+=.config/**,internal/**,src/**,cmd/**,after/**,resources/**,test/**,doc/**,acceptance/**,spec/**,include/**,colors/**,autoload/**,plugin/**,ftplugin/**,syntax/**,examples/**,pkg/**
set shortmess=SaoOstTWIcqF " shorten all messages
set showcmd
set statusline=%{repeat('—',winwidth('%'))}
set switchbuf=useopen,usetab
set tabstop=2
set notermguicolors         " terminal supports 24bit colors
set undodir=~/.local/share/nvim/site/undodir
set undofile              " persistent undo history
set wildmenu
set wildmode=full
set wildoptions-=pum
set wildignore=*~
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300

set fillchars+=vert:⎟

if has('nvim')
  set fillchars+=fold:┈,eob:\      " end of buffer char
  set fillchars+=fold:\ ,eob:\      " end of buffer char
  set fillchars+=eob:\      " end of buffer char
  " set fillchars+=fold:,eob:\      " end of buffer char
  set inccommand=nosplit    " shows affect of substitute
endif

" nnoremap <silent> <leader>gf :edit <cfile><cr>
nnoremap <silent> <C-w><C-t> :tab split<cr>
tnoremap <C-w><C-h> <C-\><C-N><C-w>h
tnoremap <C-w>h <C-\><C-N><C-w>h
tnoremap <C-w><C-j> <C-\><C-N><C-w>j
tnoremap <C-w>j <C-\><C-N><C-w>j
tnoremap <C-w><C-k> <C-\><C-N><C-w>k
tnoremap <C-w>k <C-\><C-N><C-w>k
tnoremap <C-w><C-l> <C-\><C-N><C-w>l
tnoremap <C-w>l <C-\><C-N><C-w>l
tnoremap gt <C-\><C-N>:tabnext<cr>
tnoremap gT <C-\><C-N>:tabprevious<cr>
" noremap <silent> <A-=> :resize +1<CR>
" noremap <silent> <A--> :resize -1<CR>
" noremap <silent> <A-+> :vertical resize +1<CR>
" noremap <silent> <A-_> :vertical resize -1<CR>
tnoremap <C-]> <C-\><C-n>
nnoremap <silent> <leader>tj :below split\|terminal<cr>
nnoremap <silent> <leader>tk :top split\|terminal<cr>
nnoremap <silent> <leader>tl :rightbelow vertical split\|terminal<cr>
nnoremap <silent> <leader>th :vertical split\|terminal<cr>
nnoremap <silent> <leader>tT :tabnew\|terminal<cr>
nnoremap <silent> zz zz11<c-e>
" nnoremap <silent> <a-]> zl
" nnoremap <silent> <a-[> zh

function! ProjectNotes()
  let l:project_note_dir=$HOME.'/Notes/'.fnamemodify('.', ':p:h:t')
  call mkdir(l:project_note_dir, 'p')
  execute ':tabnew '.l:project_note_dir.'/project.adoc|tcd '.l:project_note_dir
endfunction

nnoremap <silent><leader>pp :call ProjectNotes()<cr>

command! -nargs=1 Grep execute ':silent lgrep! '.<q-args> | execute ':lopen'

nnoremap <silent> <leader>bd :bp\|bd#<cr><c-g>

" augroup tmux
"   autocmd!
"   if exists('$TMUX')
"     autocmd BufReadPost,FileReadPost,BufNewFile,BufEnter * call system("tmux rename-window '" . expand("%:t") . "'")
"   endif
" augroup END

function! <SID>HighlightGroups()
  source $VIMRUNTIME/syntax/hitest.vim
endfunc

function! <SID>HighlightAtCursor()
  echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
	\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
	\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"
endfunc

command! HighlightGroups call <SID>HighlightGroups()
command! HighlightAtCursor call <SID>HighlightAtCursor()
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" nnoremap <silent> <leader>j ]c
" nnoremap <silent> <leader>k [c
" nnoremap <silent> gdh :diffget //2<cr>
" nnoremap <silent> gdl :diffget //3<cr>

function! NeatFoldText() "{{{2
  let indentLevel = indent(v:foldstart)
  let indent = repeat(' ', indentLevel)
  " let line = getline(v:foldstart)
  let line = substitute(getline(v:foldstart),'^\s*','','')
  " let line = substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g')
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
  let foldtext = line . '  '
  let foldtextstart = repeat('~', (winwidth(0)-strlen(foldtext))/2)
  " let foldtext = foldtextstart . foldtext
  let foldtextend = repeat('𐬹', winwidth(0))
  return indent . foldtext . foldtextend
endfunction
set foldtext=NeatFoldText()

tnoremap <C-l> <C-\><C-n>:call ClearTerminal()<cr>

function! ClearTerminal()
  set scrollback=1
  let &g:scrollback=1
  " echo &scrollback
  " call feedkeys("\i")
  " call feedkeys("clear\<CR>")
  " call feedkeys("\<C-\>\<C-n>")
  " call feedkeys("\i")
  sleep 100m
  let &scrollback=0 "s:scroll_value
endfunction

nnoremap <Leader>s :nohlsearch<cr>
