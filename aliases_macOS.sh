

# when using tmux this is needed
alias pbcopy="reattach-to-user-namespace pbcopy"
alias open="reattach-to-user-namespace open"

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

# notes
# alias notes='vim ~/Documents/notes/'
alias notes='vim /Users/cole/Library/Mobile\ Documents/com\~apple\~CloudDocs/notes'

# change to common directories
alias 'onedrive'='cd ~/OneDrive\ -\ cchmc'
alias 'icloud'='cd /Users/cole/Library/Mobile\ Documents/com\~apple\~CloudDocs'
