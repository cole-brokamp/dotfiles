#!/bin/bash

echo "Installing dotfiles"

dir=~/dotfiles
cd $dir

echo -e "\nCreating symlinks"
echo "=============================="

linkables=( "bash_profile" "git/gitconfig" "git/gitignore_global" "hushlogin" "nanorc" "screenrc" "tmux/tmux.conf" "Rprofile")

for file in "${linkables[@]}" ; do
    echo "Creating symlink for $file"
    target="$HOME/.$( basename $file)"
    ln -sf $dir/$file $target
done


echo -e "\n\ninstalling to ~/.config"
echo "=============================="

if [ -d ~/.config/vimrc ]; then
    echo "Deleting old ~/.config/nvim"
    rm -rf ~/.config/nvim
fi

if [ ! -d ~/.config/vimrc ]; then
    echo "Creating ~/.config/nvim"
    mkdir -p ~/.config/nvim
fi

for config in $dir/config/nvim/*; do
    target=$HOME/.config/nvim/$( basename $config )
    echo "Creating symlink for $config"
    ln -sf $config $target
done

echo -e "\n\ninstalling to ~/.nano"
echo "=============================="

if [ -d ~/.nano ]; then
    echo "Deleting old ~/.nano/"
    rm -rf ~/.nano
fi

if [ ! -d ~/.nano ]; then
    echo "Creating ~/.nano/"
    mkdir -p ~/.nano
fi

for file in $dir/nano/*; do
    target=$HOME/.nano/$( basename $file )
    echo "Creating symlink for $file"
    ln -sf $file $target
done

if [ ! -d ~/.vim/bundle/Vundle.vim ]; then
    echo -e "/n/ninstalling Vundle"
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
fi

echo -e "/n/ninstalling vim plugins"
echo "=============================="
vim +PluginInstall +qall

echo -e "/n/ninstalling vimrc"
ln -sf $dir/vimrc ~/.vimrc


echo "=============================="

if [ "$(uname)" == "Darwin" ]; then
    echo -e "\n\nRunning on OSX"
    read -p "Do you want to brew command line applications? "
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        source install/brew.sh
    fi
    echo "=============================="
    read -p "Do you want to set macOS options? "
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        source install/macOS.sh
    fi
fi

echo "Done. Reload the shell."

