# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/usr/local/opt/fzf/shell/completion.zsh" 2> /dev/null

# Note: rebuild custom bat theme:
#   bat cache --build

# Key bindings
# ------------
source "/usr/local/opt/fzf/shell/key-bindings.zsh"

# FZF_TMUX=1

# FZF_TAB_COMMAND=(
#     fzf-tmux
#     --ansi   # Enable ANSI color support, necessary for showing groups
#     --expect='$continuous_trigger' # For continuous completion
#     '--color=hl:$(( $#headers == 0 ? 108 : 255 ))'
#     --nth=2,3 --delimiter='\x00'  # Don't search prefix
#     --layout=reverse --height=${FZF_TMUX_HEIGHT:=75%}
#     --tiebreak=begin -m --bind=tab:down,btab:up,change:top,tab:toggle --cycle
#     --print-query
#     '--query=$query'   # $query will be expanded to query string at runtime.
#     '--header-lines=$#headers' # $#headers will be expanded to lines of headers at runtime
# )
# zstyle ':fzf-tab:*' command $FZF_TAB_COMMAND

HL_COLOR=#775555
export FZF_DEFAULT_OPTS="
  --info=hidden
  --ansi
  --height=70%
  --bind alt-p:previous-history
  --bind alt-n:next-history
  --bind ctrl-p:up
  --bind ctrl-n:down
  --bind alt-k:up
  --bind alt-j:down
  --bind ctrl-a:toggle-all
  --bind alt-a:toggle-all
  --color bg+:-1
  --color bg:-1
  --color fg+:-1
  --color fg:-1
  --color hl+:$HL_COLOR
  --color header:$HL_COLOR
  --color hl:$HL_COLOR
  --color info:$HL_COLOR
  --color marker:$HL_COLOR
  --color pointer:$HL_COLOR
  --color prompt:$HL_COLOR
"

fzf_preview_opts="
  --info=hidden
  --ansi
  --height=70%
  --bind alt-p:previous-history
  --bind alt-n:next-history
  --bind ctrl-p:up
  --bind ctrl-n:down
  --bind alt-k:up
  --bind alt-j:down
  --bind ctrl-a:toggle-all
  --bind alt-a:toggle-all
  --color bg+:-1
  --color bg:-1
  --color fg+:-1
  --color fg:-1
  --color hl+:$HL_COLOR
  --color header:$HL_COLOR
  --color hl:$HL_COLOR
  --color info:$HL_COLOR
  --color marker:$HL_COLOR
  --color pointer:$HL_COLOR
  --color prompt:$HL_COLOR
  --preview-window up
  --preview 'cat {} | head -200'
"
export FZF_DEFAULT_COMMAND='fd --type file --color=never --follow'
#export FZF_DEFAULT_COMMAND='fd --type file --color=always --follow --hidden --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="$fzf_preview_opts"
export FZF_CTRL_R_OPTS="--preview-window hidden --height=7"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"

