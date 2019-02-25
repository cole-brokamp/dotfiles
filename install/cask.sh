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

# fonts
brew tap caskroom/fonts
brew cask install font-hack
brew cask install font-roboto
brew cask install font-roboto-condensed
brew cask install font-computer-modern
brew cask install font-source-code-pro

### install gui apps
brew tap caskroom/cask
brew cask install Caskroom/cask/mactex
brew cask install 1password
brew cask install backblaze
brew cask install docker
brew cask install dropbox
brew cask install firefox
brew cask install kindle
brew cask install kitematic
brew cask install macdown
brew cask install mactex
brew cask install microsoft-office
brew cask install middleclick
brew cask install onedrive
brew cask install r-app
brew cask install rstudio
# brew cask install caskroom/versions/rstudio-preview
brew cask install screens-connect # install screens from app store
brew cask install texstudio
brew cask install the-unarchiver
brew cask install transmit
brew cask install vlc
brew cask install xpra
brew cask install xquartz
brew cask install texstudio

brew cask cleanup

echo -e "/n/nDone!\n\n"
