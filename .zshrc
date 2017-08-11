#
# ~/.zshrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='%m:%~ %n\$ '
# PROMPT='%(?.%F{magenta}.%F{red})$%f '
SAVEHIST=4000
HISTSIZE=2000
HISTFILE="$HOME/.zsh_history"
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.

# Taylor's University's UNIX server details
icampusip="10.99.75.211"
ocampusip="180.200.233.21"
campusport="12322"
alias icampus="ssh 0324266@10.99.75.213 -p 12322" 
alias ocampus="ssh 0324266@180.200.233.21 -p 12322"

# Cleanup emacs junk
alias cleanup="rm *(*~|a.out)"

# Don't remember entered commands until exit
alias youreold="unset HISTFILE"

# function tree {
#     find ${1:-.} -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'
# }

alias ls='ls --color=auto'

function md () { mkdir -p "$@" && cd "$@"; }

autoload -U promptinit; promptinit
prompt pure
source ~/Git/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
