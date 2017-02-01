#!/bin/bash

echo "updating homebrew..."
brew update
echo "upgrading formulas..."
brew upgrade
echo "cleaning up..."
brew cleanup
