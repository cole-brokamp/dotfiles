#!/bin/bash

if test ! $(which brew); then
    echo "Installing homebrew"
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew update
brew upgrade
brew cleanup
brew tap caskroom/cask

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
brew install cmake
brew install node
brew install less
brew install wget
brew install git
brew install gpg # for signing gh releases
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
brew cask install emacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
brew install poppler
brew install automake
brew install markdown
brew install ispell
brew install ripgrep

# science
brew install r
brew install pandoc
brew install pandoc-citeproc
brew install python
brew install python3
# brew install homebrew/science/hdf4 # still need hdf4 support for MODIS
# brew link --overwrite hdf4
brew install gdal
brew install udunits
# brew tap osgeo/osgeo4mac
# brew install qgis

echo -e "\n\nInstalling homebrew cask packages..."
echo "=============================="

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
# brew cask install onedrive
R -e "install.packages('tinytext'); tinytex::install_tinytex()"
# brew cask install screens-connect
brew cask install screens
brew cask install the-unarchiver
brew cask install transmit
brew cask install expandrive
brew cask install vlc
# brew cask install xpra
brew cask install xquartz
brew cask install pdf-expert
brew cask install bartender
brew cask install itsycal
brew cask install macdown
# install SystemPal from the app store
brew cask install spotmenu

brew cask cleanup

echo -e "/n/nDone!\n\n"

brew cleanup
