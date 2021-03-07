export PATH=/usr/local/bin:$PATH
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:$HOME/go/bin"
export PATH="$HOME/.rbenv/bin:$PATH"
export PATH="$HOME/code/emacs/nextstep/Emacs.app/Contents/MacOS/bin:$PATH"
export PATH="$HOME/code/emacs/nextstep/Emacs.app/Contents/MacOS:$PATH"
export GOPATH=~/go
export EDITOR=nvim
export ALTERNATE_EDITOR=""
alias emacs=Emacs

if [ "$INSIDE_EMACS" != "" ]
then
    export MANPAGER='vim -c MANPAGER -'
fi

test -f ~/.localenv && source ~/.localenv

PROMPT='❯ '
