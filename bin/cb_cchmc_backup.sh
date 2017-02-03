#!/bin/bash

# connect to drive (this relies on Finder)
  # con: must have Finder running
  # pro: uses stored username/password
open 'smb://rds6.cchmc.org/DBE-64/CB'

# rsync $HOME to this backup folder
rsync -a --delete --progress /Users/cole/Desktop/ /Volumes/CB/Desktop && \
rsync -a --delete --progress /Users/cole/Biostatistics/ /Volumes/CB/Biostatistics && \
rsync -a --delete --progress /Users/cole/Documents/ /Volumes/CB/Documents && \
rsync -a --delete --progress /Users/cole/dotfiles/ /Volumes/CB/dotfiles && \
rsync -a --delete --progress /Users/cole/Downloads/ /Volumes/CB/Downloads