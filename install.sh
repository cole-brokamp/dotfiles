#!/bin/bash

dir=~/dotfiles
cd $dir

# create symlinks
for file in "bash_profile" "gitconfig" "hushlogin" "nanorc" "screenrc"; do
    ln -sf $dir/$file ~/.$file 2>/dev/null
done

# manually copy over nano folder
cp -rf ~/$dir/nano/ ~/.nano/

