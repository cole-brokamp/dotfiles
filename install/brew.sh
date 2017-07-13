#!/bin/bash

if test ! $(which brew); then
    echo "Installing homebrew"
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)"
    # temporarily set path so brew is usable on linux
    PATH="$HOME/.linuxbrew/bin:$PATH"
fi

brew update
brew upgrade
brew cleanup

brew install gcc

brew tap homebrew/versions

echo -e "\n\nInstalling homebrew packages..."
echo "=============================="

brew install bash
# Switch to using brew-installed bash as default shell
if ! fgrep -q '/usr/local/bin/bash' /etc/shells; then
  echo '/usr/local/bin/bash' | sudo tee -a /etc/shells;
  chsh -s /usr/local/bin/bash;
fi;

# completion
brew install bash-completion
brew link --overwrite bash-completion
brew install docker-completion
brew install brew-cask-completion
brew install docker-compose-completion

# fonts
brew tap caskroom/fonts
brew cask install font-hack
brew cask install font-roboto
brew cask install font-roboto-condensed

# install gnu coreutils with a g prefixed
brew install coreutils

# tools
brew tap homebrew/dupes
brew install less
brew install tree
brew install wget
brew install git
brew install tmux
brew install reattach-to-user-namespace
# brew install highlight # not installable on linux
brew install diff-so-fancy
brew install pv
brew install ssh-copy-id
brew install grep --with-default-names
brew install openssh
brew install screen
brew install nano
brew install htop-osx # mac only
brew install sshuttle
brew install youtube-dl
brew install asciinema
brew install ffmpeg
brew install imagemagick
brew install lame
brew install googler
brew install tldr
brew install awscli
brew install vim --with-override-system-vi
brew install socat

# science
brew install pandoc
brew install pandoc-citeproc
brew install python

# geoinformatics
brew tap osgeo/osgeo4mac
# brew install gdal
brew install proj
brew install geos
brew install udunits
brew install postgis # takes care of liblwgeom issue
brew install gdal2 --with-armadillo --with-complete --with-libkml --with-unsupported
brew link --force gdal2

# node
brew install node
npm install -g how2
npm install -g fkill-cli
npm install -g emoj
npm install -g instant-markdown-d
npm install -g tmux-cpu
npm install -g tmux-mem

brew cleanup
