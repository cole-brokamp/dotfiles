#!/bin/bash

if test ! $(which brew); then
    echo "Installing homebrew"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> /Users/broeg1/.bash_profile
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

brew update
brew upgrade
brew cleanup

echo -e "\n\nInstalling homebrew packages..."
echo "=============================="

brew install bash
# Switch to using brew-installed bash as default shell
if ! fgrep -q '/bin/bash' /etc/shells; then
  echo '/bin/bash' | sudo tee -a /etc/shells;
  chsh -s /bin/bash;
fi;

# completion
brew install bash-completion

# tools
brew install coreutils
brew install cmake
brew install node
brew install less
brew install wget
brew install git
brew tap microsoft/git
brew install --cask git-credential-manager-core
brew install gpg # for signing gh releases
brew install tmux
brew install highlight # mac only
brew install diff-so-fancy
brew install grep
brew install openssh
brew install mosh
brew install htop-osx # mac only
brew install sshuttle
brew install awscli
brew install vim
# brew install socat

# a/v
brew install asciinema
brew install ffmpeg
brew install imagemagick
brew install lame
brew install youtube-dl

# emacs stuffs
brew tap d12frosted/emacs-plus
# brew install emacs-plus@28 --with-modern-icon --with-imagemagick --with-xwidgets
brew install emacs-plus@29 --with-xwidgets --with-no-frame-refocus --with-modern-icon --with-imagemagick
# do symmlink too!
brew install poppler
brew install automake
brew install markdown
brew install ispell
brew install ripgrep

# science
brew install r
brew install pandoc
brew install python3
brew install pkg-config
brew install gdal
brew install udunits

echo -e "\n\nInstalling homebrew cask packages..."
echo "=============================="

# quick look plugins for developers
# mac only
brew install qlcolorcode qlmarkdown \
     quicklookase
	qlstephen qlvideo quicklook-json \
	qlprettypatch quicklook-csv \
	qlimagesize webpquicklook
# get these working in later macOS versions with:
# xattr -d -r com.apple.quarantine ~/Library/QuickLook

# fonts
brew install homebrew/cask-fonts/font-hack
brew install homebrew/cask-fonts/font-source-code-pro
brew install homebrew/cask-fonts/font-raleway

### install gui apps
brew install 1password
brew install homebrew/cask/docker
brew install firefox
R -e "install.packages('tinytex'); tinytex::install_tinytex()"
# *OR* brew install --cask mactex-no-gui
brew install the-unarchiver
brew install transmit
# install pdf-expert from the app store
brew install bartender
# install SystemPal from the app store
brew install zoom
brew install microsoft-teams
# install Multitouch app from https://multitouch.app
brew install --cask rectangle

## optional
brew install backblaze
brew install microsoft-office
# brew install screens-connect
brew install screens
brew install vlc
# brew install xpra
brew install xquartz

brew cask cleanup

echo -e "/n/nDone!\n\n"

brew cleanup
