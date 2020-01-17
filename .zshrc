#
# ~/.zshrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='%m:%~ %n\$ '
# PROMPT='%(?.%F{magenta}.%F{red})$%f '
SAVEHIST=11000
HISTSIZE=10000
HISTFILE="$HOME/.zsh_history"
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt COMPLETE_ALIASES

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

case "$OSTYPE" in
    linux*)
        alias ls='ls --color=auto'
        ;;
esac

function md () { mkdir -p "$@" && cd "$@"; }

function req_add() {
    pip install $1
    local str=$(pip freeze | grep "$1")
    echo $str >> requirements.txt
}

function jest_target() {
    local file=$1
    set NODE_ICU_DATA=node_modules/full-icu& ./node_modules/.bin/react-scripts test --env=jsdom "$1" --coverage --collectCoverageFrom="$(dirname $1)/!(index|*story.).js" --watch
}

alias grepr='grep --color -nR'

fpath=( "$HOME/Git/dotfiles/zsh/zfunctions" $fpath )
zstyle ':completion:*:*:git:*' script $HOME/dotfiles/zsh/zfunctions/git-completion.zsh
autoload -Uz compinit
compinit
autoload -U promptinit; promptinit
prompt pure
autoload -U select-word-style
select-word-style bash
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
source $HOME/Git/dotfiles/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh


