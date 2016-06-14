#!/bin/sh
ln -s ~/dotfiles/.vim ~/.vim
ln -s ~/dotfiles/.vimrc ~/.vimrc

git clone https://github.com/VundleVim/Vundle.vim ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

sudo apt-get install -y cmake
python ~/dotfiles/.vim/bundle/YouCompleteMe/install.py
