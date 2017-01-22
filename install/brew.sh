#!/bin/bash

if test ! $(which brew); then
    echo "Installing homebrew"
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew update
brew upgrade
brew cleanup

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
brew install bash-completion2
brew install homebrew/completions/brew-cask-completion
brew install docker-completion

# tools
brew install tree
brew install wget
brew install git
brew install reattach-to-user-namespace
brew install tmux
brew install highlight
brew install markdown
brew install diff-so-fancy
brew install neovim/neovim/neovim
brew install pv
brew install ssh-copy-id
brew install homebrew/dupes/grep
brew install homebrew/dupes/openssh
brew install homebrew/dupes/screen
brew install nano
brew install htop-osx
brew install sshuttle
brew install youtube-dl
brew install sqlite
brew install asciinema
brew install emojify
brew install ffmpeg
brew install imagemagick
brew install lame
brew install googler
brew install tldr
brew install awscli

# science
brew install pandoc
brew install pandoc-citeproc
brew install python
brew install gdal
brew install geos
brew install proj

# node stuff
brew install node
npm install -g how2

# quick look plugins for developers
brew cask install qlcolorcode qlmarkdown \
    quicklook-json qlprettypatch quicklook-csv betterzipql \
        qlimagesize webpquicklook

brew cleanup
