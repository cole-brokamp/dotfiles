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
brew cask install middleclick
brew cask install xtrafinder
brew cask install caprine #fb messenger
brew cask install cumulus
brew cask install vlc
brew cask install github-desktop
brew cask install qgis
brew cask install duet
brew cask install kindle
brew cask install microsoft-office
brew cask install docker
brew cask install rstudio
brew cask install r-app
brew cask install the-unarchiver
brew cask install sublime-text

brew cleanup

echo -e "If installing from scratch, install Airmail 3 from the App Store"