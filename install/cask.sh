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
# brew cask install xtrafinder # unstable on sierra
brew cask install caprine #fb messenger
brew cask install vlc
brew cask install github-desktop
brew cask install qgis
# brew install --no-sandbox qgis/qgisdev/qgis3-dev
brew cask install duet
brew cask install kindle
brew cask install microsoft-office
brew cask install docker
# brew cask install rstudio
brew cask install caskroom/versions/rstudio-preview
brew cask install r-app
brew cask install the-unarchiver
brew cask install sublime-text
brew cask install macdown
brew cask install 1password
brew cask install xquartz
brew cask install kitematic
brew cask install backblaze
brew cask install bibdesk
brew cask install screens-connect # install screens from app store
brew cask install onedrive
brew cask install dropbox

brew cask cleanup

echo -e "/n/nDone!\n\n"
