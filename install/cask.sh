#!/bin/h

if test ! $(which brew); then
    echo "Installing homebrew"
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew update
brew upgrade
brew cleanup

echo -e "\n\nInstalling homebrew cask packages..."
echo "=============================="

brew tap caskroom/cask

# quick look plugins for developers
# mac only
brew cask install qlcolorcode qlmarkdown \
	qlstephen qlvideo quicklook-json \
	qlprettypatch quicklook-csv \
	qlimagesize webpquicklook

# fonts
brew tap caskroom/fonts
brew cask install font-hack
brew cask install font-source-code-pro

### install gui apps
brew cask install 1password
brew cask install backblaze
brew cask install docker
brew cask install dropbox
brew cask install firefox
brew cask install kindle
brew cask install kitematic
brew cask install microsoft-office
brew cask install middleclick
brew cask install onedrive
brew cask install r-app
R -e "install.packages('tinytext'); tinytex::install_tinytex()"
# brew cask install screens-connect
brew cask install screens
# brew cask install the-unarchiver
brew cask install transmit
brew cask install vlc
brew cask install xpra
brew cask install xquartz
brew cask install pdf-expert
brew cask install bartender
brew cask install itsycal
brew cask install macdown
# install SystemPal from the app store

brew cask cleanup

echo -e "/n/nDone!\n\n"
