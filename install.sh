#!/bin/bash

dir=~/dotfiles
olddir=~/.dotfiles_old 
files=`ls | grep -v "makeSymLinks.sh" | grep -v "README.md" | grep -v "CB.terminal"` | grep -v "install.sh"  

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles"
mkdir -p $olddir

cd $dir

# move any existing dotfiles in homedir to dotfiles_old directory
# then create symlinks from the homedir to any files in the ~/dotfiles directory specified in $files
for file in $files; do
    if [ -f ~/.$file ]; then
        mv -f ~/.$file $olddir 2>/dev/null
        echo "Backing up $file to $olddir"
    fi
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/.$file 2>/dev/null
done

