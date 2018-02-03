#!/bin/bash

if test ! $(which python3); then
    echo "python3 not installed... "
    exit
fi

echo -e "\n\nInstalling pip3 packages..."
echo "=============================="

sudo -H pip3 install -U git+https://github.com/randy3k/rice
sudo -H pip3 install neovim # neovim python3 client
