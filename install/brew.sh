#!/bin/bash

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew update
brew upgrade
brew cleanup

brew install bash
echo '/opt/homebrew/bin/bash' | sudo tee -a /etc/shells
chsh -s /opt/homebrew/bin/bash

brew install coreutils
brew install findutils
brew install gnu-sed
brew install grep
brew install diffutils

brew install gcc
brew install cmake
brew install less
brew install wget
brew install git
brew install openssh
brew install htop
brew install neovim
brew install just
brew install shfmt
brew install watchexec

brew install imagemagick
brew install r
brew install pandoc
brew install python3
brew install pkgconf
brew install gdal
brew install udunits
brew install libgit2

# fonts
brew install --cask font-hack
brew install --cask font-source-code-pro

brew cleanup
