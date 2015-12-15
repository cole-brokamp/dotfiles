#!/bin/bash

dir=~/dotfiles
olddir=~/dotfiles_old 
files=`ls | grep -v "makeSymLinks.sh" | grep -v "README.md" | grep -v "CB.terminal"`  

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles in ~ ..."
mkdir -p $olddir

cd $dir

# move any existing dotfiles in homedir to dotfiles_old directory
# then create symlinks from the homedir to any files in the ~/dotfiles directory specified in $files
for file in $files; do
    mv ~/.$file ~/dotfiles_old/
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/.$file
done

# remove the extra created nano folder
rm -r ~/dotfiles/nano/nano
