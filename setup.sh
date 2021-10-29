#!/bin/sh

stow -t $HOME -S stow -v


wget https://raw.githubusercontent.com/rupa/z/master/z.sh -O ~/.z.sh
ln -s  $HOME"/ownCloud/org" $HOME"/org"
