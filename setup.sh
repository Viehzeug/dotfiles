#!/bin/sh

SRC_DIR=$(pwd)

ln -s  $SRC_DIR"/zsh/.zsh" $HOME"/.zsh"
ln -s  $SRC_DIR"/zsh/.zlogin" $HOME"/.zlogin"
ln -s  $SRC_DIR"/zsh/.zshrc" $HOME"/.zshrc"
ln -s  $SRC_DIR"/zsh/.zprofile" $HOME"/.zprofile"
ln -s  $SRC_DIR"/.profile" $HOME"/.profile"


wget https://raw.githubusercontent.com/rupa/z/master/z.sh -O ~/.z.sh
