#!/bin/bash

echo "Installing dotfiles"

# exit on error
set -e

dir=~/dotfiles
cd $dir

echo -e "\nCreating symlinks"
echo "=============================="

linkables=( "bash_profile" "bashrc" "gitconfig" "gitignore" "hushlogin" "nanorc" "screenrc" "tmux.conf" "vimrc" "Rprofile" "inputrc")

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

if [ ! -d ~/.vim/bundle/Vundle.vim ]; then
    echo -e "\n\ninstalling Vundle"
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
fi

echo -e "\n\ninstalling vim plugins"
echo "=============================="
# vim --not-a-term +PluginInstall +qall
echo | echo | vim +PluginInstall +qall &>/dev/null # hacky workaround for no UI install of plugins

echo "Done. Reload the shell."

