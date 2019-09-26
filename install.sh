#!/bin/bash

echo "Installing dotfiles"

# exit on error
set -e

dir=~/dotfiles
cd $dir

echo -e "\nCreating symlinks"
echo "=============================="

linkables=( "bash_profile" "bashrc" "gitconfig" "gitignore" "hushlogin" "screenrc" "tmux.conf" "vimrc" "Rprofile" "inputrc" "spacemacs")

for file in "${linkables[@]}" ; do
    echo "Creating symlink for $file"
    target="$HOME/.$( basename $file)"
    ln -sf $dir/$file $target
done

echo "Done. Reload the shell."

