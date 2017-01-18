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

### install gui apps
brew tap caskroom/cask
brew cask install Caskroom/cask/mactex
brew cask install firefox
brew cask install transmit
brew cask install transmission
brew cask install texstudio
brew cask middleclick
brew cask install xtrafinder
brew cask install caprine #fb messenger
brew cask install quik
brew cask install cumulus
brew cask vlc
brew cask github-desktop
brew cask qgis
brew cask duet
brew cask kindle
brew cask microsoft-office
brew cask docker
brew cask rstudio
brew cask r-app
cask install the-unarchiver
cask install sublime-text

brew cleanup

echo -e "If installing from scratch, install Airmail 3 from the App Store"
