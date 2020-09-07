let loaded_matchit=1
filetype plugin indent on
let mapleader=' '
let maplocalleader=','

runtime editor.vim
runtime tabline.vim

let g:plug_shallow=0 " Full git clone
let g:plug_window='tab new' " New tab for plug work

call plug#begin('~/.local/share/nvim/plugged')

Plug 'wincent/terminus'

Plug 'tpope/vim-dispatch' | runtime dispatch.vim

" Running tests
" Plug 'janko/vim-test' | runtime test.vim
Plug 'kassio/neoterm'   | runtime neoterm.vim

" LSP
runtime lsp/vim-lsp/init.vim

Plug 'matthias-margush/context.vim'

" Editor settings
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-apathy'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'

" UX
Plug 'nelstrom/vim-visual-star-search'
Plug 'matthias-margush/vim-unimpaired'

" Colors
Plug 'matthias-margush/vim-noctu'

" Editing enhancements
Plug 'ntpeters/vim-better-whitespace' | runtime better-whitespace.vim
Plug 'tommcdo/vim-lion'               | let g:lion_squeeze_spaces = 1
Plug 'tpope/vim-commentary'
Plug 'machakann/vim-sandwich'         | runtime sandwich.vim
Plug 'bhurlow/vim-parinfer'           | runtime parinfer.vim
Plug 'wellle/targets.vim'

" Plug 'scrooloose/nerdtree', {'on': ['NERDTreeToggle', 'NERDTreeFind', 'NERDTree']} | runtime nerdtree.vim
Plug 'lambdalisue/fern.vim' | runtime fern.vim
Plug 'lambdalisue/fern-renderer-nerdfont.vim'
Plug 'lambdalisue/nerdfont.vim'
Plug 'tpope/vim-eunuch'

" Workspace / sessions
Plug 'matthias-margush/vim-workspace'

" Git
Plug 'rhysd/conflict-marker.vim'
Plug 'tpope/vim-fugitive' | runtime fugitive.vim
Plug 'tpope/vim-rhubarb'
Plug 'junkblocker/patchreview-vim'
Plug 'codegram/vim-codereview'

" Docs / Markup
Plug 'plasticboy/vim-markdown', {'for': 'markdown'} | runtime markdown.vim
Plug 'iamcco/markdown-preview.nvim', {'for': 'markdown', 'do': 'cd app && yarn install'} | runtime markdown-preview.vim
Plug 'godlygeek/tabular', {'for': 'markdown'}
Plug 'habamax/vim-asciidoctor' | runtime asciidoctor.vim " gem install asciidoctor-diagram

" Presenting
Plug 'junegunn/goyo.vim' | runtime present.vim
Plug 'junegunn/limelight.vim'

" Pager
Plug 'lambdalisue/vim-manpager', {'on': 'MANPAGER'}

" Thesaurus
Plug 'Ron89/thesaurus_query.vim', {'on': 'Thesaurus'} | runtime thesaurus.vim

" Mail
"  'neomutt/neomutt.vim'
"  'soywod/iris.vim'

" Tools
Plug 'tweekmonster/startuptime.vim'
Plug 'norcalli/nvim-colorizer.lua' | nnoremap <silent> <leader>tc :ColorizerToggle<cr>
Plug 'AndrewRadev/bufferize.vim'

call plug#end()

autocmd! filetype vim-plug nnoremap gq :q<cr>

colorscheme noctu

runtime notes.vim
runtime workspace.vim

" source ~/.config/kitty/ink.vim
