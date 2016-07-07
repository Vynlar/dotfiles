#!/bin/sh
ln -s ~/dotfiles/.vim ~/.vim
ln -s ~/dotfiles/.vimrc ~/.vimrc

#YouCompleteMe
sudo apt-get install -y python-dev python3-dev build-essential cmake
#Command T
sudo apt-get install -y ruby-full

git clone https://github.com/VundleVim/Vundle.vim ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

cd ~/.vim/bundle/YouCompleteMe
./install.py --tern-completer
cd ~/dotfiles

cd ~/.vim/bundle/command-t/ruby/command-t
ruby extconf.rb
make
cd ~/dotfiles
