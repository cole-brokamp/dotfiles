#!/bin/bash

if test ! $(which node); then
    echo "node not installed"
    exit
fi

echo -e "\n\nInstalling node packages..."
echo "=============================="

npm install -g vmd # for github flavored markdown
npm install -g terminalizer
npm install -g @mermaid-js/mermaid-cli
