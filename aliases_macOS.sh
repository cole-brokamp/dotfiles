### macOS specific aliases

# when using tmux this is needed
alias pbcopy="reattach-to-user-namespace pbcopy"

# mount shared drives
alias mount_cb="open -g 'smb://rds6.cchmc.org/DBE-64/CB'"
alias mount_gis="open -g 'smb://rds6.cchmc.org/DBE-65/GIS'"

# use gnu ls for better colors
alias ls='gls -Fh --color'
alias dircolors='gdircolors'

# lock
alias lock='/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'

# cd to current finder window
alias cdf='cd "`osascript -e "tell application \\"Finder\\" to get POSIX path of (insertion location as alias)"`"'

# copy working directory
alias cwd="pwd | tr -d '\r\n' | pbcopy | echo '=> working directory copied to keyboard'"

# open file in new R Studio instance
alias rstudio='open -na "rstudio"'

# open file in Sublime
alias s='open -a "Sublime Text"'

# notes
alias notes='vim ~/Documents/notes/'

# change to common directories
alias 'biostat'='cd ~/Biostatistics'
alias 'tmp'='cd ~/Biostatistics/_TMP'
alias 'cb'='cd ~/Biostatistics/_CB'
alias 'cv'='cd ~/Documents/CV_website_more/CV'
