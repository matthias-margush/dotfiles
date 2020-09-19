if exists('g:vscode')
    runtime vscode.vim
else
  let loaded_matchit=1
  filetype plugin indent on
  let mapleader=' '
  let maplocalleader=','

  runtime editor.vim
  runtime tabline.vim

  let g:plug_shallow=0 " Full git clone
  let g:plug_window='tab new' " New tab for plug work

  call plug#begin('~/.local/share/nvim/plugged')

  " Plug 'matthias-margush/critiq.vim' | runtime critiq.vim
  " Plug 'junkblocker/patchreview-vim'
  " Plug 'codegram/vim-codereview'

  " Plug 'wincent/terminus'
  Plug 'tpope/vim-dispatch' | runtime dispatch.vim

  " Running tests
  " Plug 'janko/vim-test' | runtime test.vim
  Plug 'kassio/neoterm'   | runtime neoterm.vim
  Plug 'igemnace/vim-makery'

  " LSP
  " runtime lsp/nvim/init.vim
  runtime lsp/vim-lsp/init.vim
  " runtime lsp/coc/init.vim

  " Plug 'nvim-treesitter/nvim-treesitter'
  Plug 'matthias-margush/context.vim' | runtime context.vim

  " Editor settings
  Plug 'editorconfig/editorconfig-vim'
  Plug 'tpope/vim-apathy'
  Plug 'tpope/vim-repeat'
  Plug 'tpope/vim-sleuth'

  " UX
  Plug 'matthias-margush/vim-unimpaired'
  Plug 'nelstrom/vim-visual-star-search'
  Plug 'romainl/vim-cool'

  " Colors
  Plug 'matthias-margush/vim-noctu'

  " Editing enhancements
  Plug 'ntpeters/vim-better-whitespace' | runtime better-whitespace.vim
  Plug 'tommcdo/vim-lion'               | runtime lion.vim
  Plug 'tpope/vim-commentary'
  Plug 'machakann/vim-sandwich'         | runtime sandwich.vim
  Plug 'bhurlow/vim-parinfer'           | runtime parinfer.vim
  Plug 'wellle/targets.vim'
  Plug 'chrisbra/NrrwRgn'

  Plug 'scrooloose/nerdtree', {'on': ['NERDTreeToggle', 'NERDTreeFind', 'NERDTree']} | runtime nerdtree.vim
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
  " runtime treesitter.vim
  " runtime lsp/nvim/lsp.vim

endif
