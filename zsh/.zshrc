eval "$(starship init zsh)"

source "${HOME}/bin/antigen.zsh"

antigen use oh-my-zsh
antigen bundle vi-mode
antigen bundle Aloxaf/fzf-tab
antigen apply

source ~/.fzf.zsh
# source ~/.fzf-tab.zsh

export MODE_INDICATOR=

bindkey -v
set -o vi

test -f ~/.localrc && source ~/.localrc

e() {
  local original_title # tmux title
  if [ "$TMUX" != "" ]
  then
    original_title="$(tmux display-message -p '#W')"
  fi
  clear
  "$EDITOR" "$@"
  if [ "$TMUX" != "" ]
  then
    tmux rename-window "$original_title"
  fi
}

alias :e=e

DISABLE_AUTO_TITLE=true
function set-title() {
  echo -en "\e]2;$@\a"
}
set-title " "
export HISTSIZE=100000
export HISTFILESIZE=100000
export HISTFILE=~/.zhistory

# set tmux pane and tab title
function tmux_title_precmd() {
  tmux rename-window "zsh"
}

function tmux_title_preexec() {
  local cmd=$(echo "$1" | awk '{print $1;}')
  tmux rename-window "$cmd"
}

if [[ "$TERM" == (screen*|tmux*) ]]; then
  add-zsh-hook -Uz precmd tmux_title_precmd
  add-zsh-hook -Uz preexec tmux_title_preexec
fi

if type direnv &> /dev/null
then
  eval "$(direnv hook zsh)"
fi

function vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

function zle-keymap-select zle-line-init zle-line-finish
{
  case $KEYMAP in
      vicmd) print -n '\033[1 q';; # block
      viins|main) print -n '\033[3 q';; # underline
  esac
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select
