#!/bin/bash

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Finder: allow quitting via ⌘ + Q; doing so will also hide desktop icons
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true 
defaults write com.apple.finder QuitMenuItem -bool true
defaults write com.apple.finder AppleShowAllFiles 1
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

# Show the ~/Library folder
chflags nohidden ~/Library

# Only use UTF-8 in Terminal.app
defaults write com.apple.terminal StringEncodings -array 4

# Time Machine
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
hash tmutil &> /dev/null && sudo tmutil disablelocal

# Activity Monitor
defaults write com.apple.ActivityMonitor OpenMainWindow -bool true
defaults write com.apple.ActivityMonitor IconType -int 5
defaults write com.apple.ActivityMonitor ShowCategory -int 0
defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"
defaults write com.apple.ActivityMonitor SortDirection -int 0

for app in "Activity Monitor" "cfprefsd" "Dock" "Finder" "SystemUIServer" "Terminal"; do
  killall "${app}" &> /dev/null
done
echo "Done. Note that some of these changes require a logout/restart to take effect."
