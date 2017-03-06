### macOS specific aliases

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

# googler
alias g='googler --colors bjdxxy -n 6'

# notes
alias notes='cd /Users/cole/Library/Mobile\ Documents/com~apple~CloudDocs/notes; ls'
# todo will open todo note for school and backup when i save a copy
alias todo='nano -B --backupdir="/Users/cole/Library/Mobile\ Documents/com~apple~CloudDocs/notes/cchmc/" /Users/cole/Library/Mobile\ Documents/com~apple~CloudDocs/notes/cchmc/todo'

# change to common directories
alias 'biostat'='cd ~/Biostatistics'
alias 'tmp'='cd ~/Biostatistics/_TMP'
alias 'cb'='cd ~/Biostatistics/_CB'
alias 'cv'='cd ~/Documents/CV_website_more/CV'
