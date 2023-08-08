#!/bin/bash

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew update
brew upgrade
brew cleanup

brew install bash
echo '/bin/bash' | sudo tee -a /etc/shells
chsh -s /bin/bash

# tools
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
brew tap microsoft/git
brew install --cask git-credential-manager-core
# brew install tmux
brew install diff-so-fancy
brew install grep
brew install openssh
brew install mosh
brew install htop
# brew install sshuttle
brew install awscli
brew install vim

# a/v
# brew install asciinema
# brew install ffmpeg
brew install imagemagick
# brew install lame

# emacs stuffs
brew install poppler
brew install automake
brew install markdown
brew install ispell
brew install ripgrep
brew install hellothisisflo/the-tap/vmd # vmd on mac
brew install node
npm install -g @mermaid-js/mermaid-cli
brew tap d12frosted/emacs-plus
brew install emacs-plus@29 --with-xwidgets --with-no-frame-refocus --with-modern-icon --with-imagemagick
# do symmlink too!

# science
brew install r
brew install pandoc
brew install python3
brew install pkg-config
brew install gdal
brew install udunits
brew pin r gdal geos proj udunits
brew install homebrew/cask/docker
R -e "install.packages('tinytex'); tinytex::install_tinytex()"
# *OR* brew install --cask mactex-no-gui

# quick look plugins
brew install qlcolorcode
brew install qlmarkdown
brew install quicklookase
brew install qlstephen
brew install qlvideo
brew install quicklook-json
brew install qlprettypatch
brew install quicklook-csv
brew install qlimagesize
brew install webpquicklook
# get these working in later macOS versions with:
# xattr -d -r com.apple.quarantine ~/Library/QuickLook

# fonts
brew install homebrew/cask-fonts/font-hack
brew install homebrew/cask-fonts/font-source-code-pro

### gui apps
# install Multitouch app from https://multitouch.app (includes rectangle)
brew install backblaze
brew install transmit
brew install zoom
brew install microsoft-teams
brew install kindle
# brew install xquartz
# brew install screens-connect
# brew install screens

brew cleanup
