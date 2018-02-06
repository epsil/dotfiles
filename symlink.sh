#!/bin/sh
DOTFILES=$(cd `dirname $0` && pwd)
ln -b -s $DOTFILES/bash_aliases ~/.bash_aliases
ln -b -s $DOTFILES/bash_env ~/.bash_env
ln -b -s $DOTFILES/bashrc ~/.bashrc
ln -b -s $DOTFILES/gitconfig ~/.gitconfig
ln -b -s $DOTFILES/rcrc ~/.rcrc
ln -b -s $DOTFILES/vimrc ~/.vimrc
ln -b -s $DOTFILES/zshrc ~/.zshrc
ln -b -s $DOTFILES/emacs ~/.emacs
ln -b -s $DOTFILES/emacs.d ~/.emacs.d
