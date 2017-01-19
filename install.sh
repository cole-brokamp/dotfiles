#!/bin/bash

echo "Installing dotfiles"

dir=~/dotfiles
cd $dir

echo -e "\nCreating symlinks"
echo "=============================="

linkables=( "bash_profile" "git/gitconfig" "git/gitignore_global" "hushlogin" "nanorc" "screenrc")

for file in "${linkables[@]}" ; do
    echo "Creating symlink for $file"
    target="$HOME/.$( basename $file)"
    ln -sf $dir/$file $target
done

echo -e "\nCopying nano directory"
echo "=============================="

# manually copy over nano folder
cp -rf $dir/nano/ ~/.nano/

if [ "$(uname)" == "Darwin" ]; then
    echo -e "\n\nRunning on OSX"
    read -p "Do you want to brew command line applications? "
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        source install/brew.sh
    fi
    echo "=============================="
    read -p "Do you want to set macOS options? "
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        source install/macOS.sh
    fi
fi

echo "Done. Reload the terminal."
