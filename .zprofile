[[ -f ~/.zshrc ]] && . ~/.zshrc

# export GOPATH=$(go env GOPATH)
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export GO111MODULE=on
export PATH="$HOME/Library/Android/sdk/platform-tools:/usr/local/Cellar/emacs-mac/emacs-26.1-z-mac-7.4/bin:/usr/local/bin:/usr/local/sbin:$PATH:$GOBIN"
export LC_ALL="en_US.UTF-8"

case "$OSTYPE" in
    darwin*)
        export CLICOLOR=1
        ;;
esac

DOCKER_FILE=$HOME/.docker_containers
test -f $DOCKER_FILE && source $DOCKER_FILE
