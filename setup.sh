#!/bin/sh

SRC_DIR=$(pwd)

ln -s  $SRC_DIR"/zsh/.zsh" $HOME"/.zsh"
ln -s  $SRC_DIR"/zsh/.zlogin" $HOME"/.zlogin"
ln -s  $SRC_DIR"/zsh/.zshrc" $HOME"/.zshrc"
ln -s  $SRC_DIR"/zsh/.zprofile" $HOME"/.zprofile"
ln -s  $SRC_DIR"/.profile" $HOME"/.profile"



# emacs
#git clone https://github.com/plexus/chemacs.git ~/chemacs
#jcd ~/chemacs
#./install.sh
#ln -s  $SRC_DIR"/emacs/.emacs-profiles.el" $HOME"/.emacs-profiles.el"
#echo 'default' > ~/.emacs-profile

#wget https://raw.githubusercontent.com/rupa/z/master/z.sh -O ~/.z.sh
