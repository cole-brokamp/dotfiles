#!/bin/bash

echo -e  "\n\nupdating homebrew..."
brew update
echo -e "\n\nupgrading formulas..."
brew upgrade
echo -e "\n\ncleaning up..."
brew cleanup
