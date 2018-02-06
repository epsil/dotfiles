#!/bin/sh
DIR=$(cd `dirname $0` && pwd)
cd "$DIR"
ln -b -s ./bash_aliases ~/.bash_aliases
ln -b -s ./bash_env ~/.bash_env
ln -b -s ./bashrc ~/.bashrc
ln -b -s ./gitconfig ~/.gitconfig
ln -b -s ./rcrc ~/.rcrc
ln -b -s ./vimrc ~/.vimrc
ln -b -s ./zshrc ~/.zshrc
ln -b -s ./emacs ~/.emacs
ln -b -s ./emacs.d ~/.emacs.d
