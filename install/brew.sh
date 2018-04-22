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
brew cask install font-computer-modern
brew cask install font-source-code-pro

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
brew install highlight # mac only
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
brew tap clangen/musikcube
brew install musikcube
brew install imagemagick
brew install lame
brew install googler
brew install tldr
brew install awscli
brew install vim --with-override-system-vi
brew install nvim
brew install socat

# emacs stuffs
brew tap d12frosted/emacs-plus
brew install emacs-plus
brew linkapps emacs-plus
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
brew install poppler
brew install automake
brew install markdown
brew install ispell

# science
brew install pandoc
brew install pandoc-citeproc
brew install python
brew install python3
brew tap osgeo/osgeo4mac
brew install libkml-dev
brew install proj
brew install geos
brew install udunits
brew install postgis # takes care of liblwgeom issue
brew install homebrew/science/hdf4 # still need hdf4 support for MODIS
brew link --overwrite hdf4
brew install gdal --with-complete --with-unsupported
brew unlink gdal # unlink before installing gdal2
brew install gdal2 --with-armadillo --with-complete --with-unsupported
brew link --force gdal2

brew cleanup
