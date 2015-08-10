#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

# OS X-like bash prompt
PS1='\h:\W \u\$ '

# Taylor's University's UNIX server details
export icampusip="10.99.75.213"
export ocampusip="180.200.233.21"
export campusport="12322"
alias icampus="ssh 0324266@10.99.75.213 -p 12322" 
alias ocampus="ssh 0324266@180.200.233.21 -p 12322"

# Cleanup emacs junk
alias cleanup="rm *~ && rm a.out"

# Don't remember entered commands until exit
alias youreold="unset HISTFILE"

# Displays the tree of the current directory
if [ ! -x "$(which tree 2>/dev/null)" ]
then alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"
fi

# Enables extglob. shopt -u extglob to turn off.
shopt -s extglob

