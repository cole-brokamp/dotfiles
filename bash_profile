# show if I am in a git branch

find_git_branch() {
  # Based on: http://stackoverflow.com/a/13003854/170413
  local branch
  if branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null); then
    if [[ "$branch" == "HEAD" ]]; then
      branch='detached*'
    fi
    git_branch="($branch)"
  else
    git_branch=""
  fi
}

find_git_dirty() {
  local status=$(git status --porcelain 2> /dev/null)
  if [[ "$status" != "" ]]; then
    git_dirty='*'
  else
    git_dirty=''
  fi
}

PROMPT_COMMAND="find_git_branch; find_git_dirty; $PROMPT_COMMAND"

# shell prompt name
PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]$(scutil --get ComputerName):\[\033[33;1m\]\W\[\033[m\]\[\033[38;5;93m\]\$git_branch\$git_dirty\]\[\033[38;5;15m\]$ "

# alias for newer version of screen for mac osx
alias screen="/usr/local/bin/screen"

# alias to lock mac
alias lock='/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'

# alias to cd to current finder window
alias cdf='cd "`osascript -e "tell application \\"Finder\\" to get POSIX path of (insertion location as alias)"`"'

# alias to copy working directory
alias cwd="pwd | tr -d '\r\n' | pbcopy | echo '=> working directory copied to keyboard'"

# alias to open file in markright
alias md='open -a /Applications/Markright.app'

# alias to open file in new R Studio instance
alias rstudio='open -na "rstudio"'

# alias to open file in Sublime Text
# use "s ." to open current directory in Sublime Text
alias s='open -a "Sublime Text 2"'

# alias to view names of all of my scripts
alias myscripts='ls ~/Documents/Biostatistics/_CB/shell_scripts/'

# nano alias for options
alias nano='nano --tabstospaces --tabsize=4 --autoindent --nohelp --quickblank'

# notes alias
alias notes='cd ~/Dropbox/Cole/notes; ls'
# todo alias will open todo note for school and backup when i save a copy
alias todo='nano -B --backupdir="~Dropbox/Cole/notes/school/" ~/Dropbox/Cole/notes/school/todo'

# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU
    colorflag="--color"
else # OS X
    colorflag="-G"
fi

# ls alias
alias ls='ls -Fh ${colorflag}'
alias ll='ls -Fhal ${colorflag}'
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# grep alias
alias grep='grep --color=auto'

# R alias
alias R='R --vanilla --quiet'
alias r='R --vanilla --quiet'

# alias to change to common directories
alias 'biostat'='cd ~/Documents/Biostatistics'
alias 'tmp'='cd ~/Documents/Biostatistics/_TMP'
alias 'dissertation'='cd ~/Dropbox/Cole/UC\ dropbox/Dissertation/'
alias 'cb'='cd ~/Documents/Biostatistics/_CB'
alias 'scripts'='cd ~/Documents/Biostatistics/_CB/shell_scripts'

# tab-completion for brew
source $(brew --repository)/Library/Contributions/brew_bash_completion.sh

# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH

# MacPorts Installer addition on 2013-08-05_at_01:23:05: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH

# setting path for latex2rtf
export PATH=$PATH:/Users/cole/Documents/Users/cole/latex2rtf-2.3.8 

# add path for gdal
export PATH=$PATH:/Library/Frameworks/GDAL.framework/Versions/1.11/Programs

test -r /sw/bin/init.sh && . /sw/bin/init.sh

# setting path for my own shell scripts
export PATH=$PATH:/Users/cole/Documents/Biostatistics/_CB/shell_scripts

# setting path for pandoc
export PATH=$PATH:/Applications/RStudio.app/Contents/MacOS/pandoc/pandoc
