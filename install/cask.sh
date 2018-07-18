#!/bin/bash

if test ! $(which brew); then
    echo "Installing homebrew"
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew update
brew upgrade
brew cleanup

echo -e "\n\nInstalling homebrew cask packages..."
echo "=============================="

# quick look plugins for developers
# mac only
brew cask install qlcolorcode qlmarkdown \
	qlstephen qlvideo quicklook-json \
	qlprettypatch quicklook-csn \
	betterzipql qlimagesize webpquicklook

### install gui apps
brew tap caskroom/cask
brew cask install Caskroom/cask/mactex
brew cask install firefox
brew cask install transmit
brew cask install transmission
brew cask install texstudio
brew cask install middleclick
brew cask install vlc
# qgis 3.2 workaround for using brew installation
brew cask install --no-sandbox https://raw.githubusercontent.com/fjperini/homebrew-qgisdev/matplotlib-fix%2Bpython/Formula/qgis3-dev.rb
brew cask install duet
brew cask install kindle
brew cask install microsoft-office
brew cask install docker
brew cask install r-app
# brew cask install rstudio
brew cask install caskroom/versions/rstudio-preview
brew cask install the-unarchiver
brew cask install sublime-text
brew cask install macdown
brew cask install 1password
brew cask install xquartz
brew cask install kitematic
brew cask install backblaze
# brew cask install bibdesk
brew cask install screens-connect # install screens from app store
brew cask install onedrive
brew cask install dropbox

brew cask cleanup

echo -e "/n/nDone!\n\n"
