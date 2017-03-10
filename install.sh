#!/bin/bash

echo "Installing dotfiles"

# exit on error
set -e

dir=~/dotfiles
cd $dir

echo -e "\nCreating symlinks"
echo "=============================="

linkables=( "bash_profile" "gitconfig" "gitignore" "hushlogin" "nanorc" "screenrc" "tmux.conf" "Rprofile")

for file in "${linkables[@]}" ; do
    echo "Creating symlink for $file"
    target="$HOME/.$( basename $file)"
    ln -sf $dir/$file $target
done

if [ ! -d ~/.tmux/plugins/tpm ]; then
    echo -e "\n\ninstalling tmux plugin manager"
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

echo -e "\n\ninstalling, updating, and cleaning tmux plugins"
echo "=============================="

~/.tmux/plugins/tpm/bin/install_plugins
~/.tmux/plugins/tpm/bin/update_plugins all
~/.tmux/plugins/tpm/bin/clean_plugins


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
    echo -e "\n\ninstalling Vundle"
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
fi

echo -e "\n\ninstalling vim plugins"
echo "=============================="
vim +PluginInstall +qall

echo -e "\n\ninstalling vimrc"
ln -sf $dir/vimrc ~/.vimrc

echo -e "\n\ninstalling vim plugins again just in case theme wasn't recognized"
echo "=============================="
vim +PluginInstall +qall


echo -e "\n\n installing brew and its packages"
echo "=============================="
install/brew.sh

if [ "$(uname)" == "Darwin" ]; then
    echo -e "\n\n=============================="
    echo -e "\n\nIf running on macOS, you may want to"
    echo -e "\n brew cask gui apps with install/cask.sh"
    echo -e "\n and/or set macOS options with install/macOS.sh"
    echo -e "===============================\n\n"
fi

echo "Done. Reload the shell."

