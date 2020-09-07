
" if exists("golangci-lint")
" 	finish
" endif
" let current_compiler = "golangci-lint"

" if exists(":CompilerSet") != 2
" 	command -nargs=* CompilerSet setlocal <args>
" endif

" " From https://github.com/fatih/vim-go/blob/32026717bd548db8274ba6ca309b648cd779396d/autoload/go/lint.vim#L442-L445
" " Golangci-lint can output the following:
" "   <file>:<line>:<column>: <message> (<linter>)
" " This can be defined by the following errorformat:
" CompilerSet makeprg=golangci-lint\ run\ ./...
" " CompilerSet errorformat='level=%tarning\ msg="%m:\ [%f:%l:%c:\ %.%#]",level=%tarning\ msg="%m",level=%trror\ msg="%m:\ [%f:%l:%c:\ %.%#]",level=%trror\ msg="%m",%f:%l:%c:\ %m,%f:%l\ %m'
" CompilerSet errorformat&
