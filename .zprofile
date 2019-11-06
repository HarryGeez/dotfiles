[[ -f ~/.zshrc ]] && . ~/.zshrc

# export GOPATH=$(go env GOPATH)
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export PATH="$HOME/Library/Android/sdk/platform-tools:/opt/local/libexec/gnubin:$HOME/Applications/MacPorts/EmacsMac.app/Contents/MacOS:/opt/local/bin:/opt/local/sbin:/opt/local/lib/mysql56/bin:$PATH:$GOBIN"
export LC_ALL="en_US.UTF-8"

DOCKER_FILE=$HOME/.docker_containers
test -f $DOCKER_FILE && source $DOCKER_FILE
