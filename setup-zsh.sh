#!/bin/sh

SCRIPT=`realpath $0`
SCRIPTPATH=`dirname $SCRIPT`

# chsh -s $(which zsh)
chsh -s /bin/zsh

cd $HOME
ln -s "$SCRIPTPATH/.zprofile"
ln -s "$SCRIPTPATH/.zshrc"

git submodule update --init --recursive
