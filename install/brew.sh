#!/bin/bash

if test ! $(which brew); then
    echo "Installing homebrew"
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew update
brew upgrade
brew cleanup

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


# install gnu coreutils with a g prefixed
brew install coreutils

# tools
brew install node
brew install less
brew install wget
brew install git
brew install tmux
brew install highlight # mac only
brew install diff-so-fancy
brew install grep
brew install openssh
brew install mosh
brew install htop-osx # mac only
brew install sshuttle
brew install youtube-dl
brew install asciinema
brew install ffmpeg
brew install imagemagick
brew install lame
# brew install awscli
brew install vim
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
brew install r
brew install pandoc
brew install pandoc-citeproc
brew install python
brew install python3
# brew install homebrew/science/hdf4 # still need hdf4 support for MODIS
# brew link --overwrite hdf4
brew install gdal
# brew tap osgeo/osgeo4mac
# brew install qgis

brew cleanup
