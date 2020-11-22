export PATH=/usr/local/bin:$PATH
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:$HOME/go/bin"
export PATH="$HOME/.rbenv/bin:$PATH"
export GOPATH=~/go

export EDITOR=nvim
export ALTERNATE_EDITOR=""


if [ "$INSIDE_EMACS" != "" ]
then
    export MANPAGER='vim -c MANPAGER -'
fi

test -f ~/.localenv && source ~/.localenv

