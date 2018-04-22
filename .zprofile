[[ -f ~/.zshrc ]] && . ~/.zshrc

# MacPorts Installer addition on 2015-10-02_at_07:44:37: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/libexec/gnubin:/opt/local/bin:/opt/local/sbin:/opt/local/lib/mysql56/bin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

export PATH="$HOME/Library/Android/sdk/platform-tools:$PATH"
alias emacs='~/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs'
source .docker_containers
