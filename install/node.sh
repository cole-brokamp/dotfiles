#!/bin/bash

if test ! $(which node); then
    echo "node not installed"
    exit
fi

echo -e "\n\nInstalling node packages..."
echo "=============================="

npm install -g how2
npm install -g fkill-cli
npm install -g emoj
npm install -g ttystudio
npm install -g vmd # for github flavored markdown
