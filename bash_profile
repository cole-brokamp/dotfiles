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
PROMPT_DIRTRIM=5
PS1="\[\033[36m\]\u\[\033[m\] @ \[\033[32m\]$(scutil --get ComputerName) \[\033[m\][\[\033[33;1m\]\w\[\033[m\]]\[\033[m\]\[\033[38;5;93m\] \$git_branch\$git_dirty\]\[\033[38;5;15m\]\\n  > "

# bash completion
# install more at https://github.com/Homebrew/homebrew-completions
if [ -f $(brew --prefix)/etc/bash_completion ]; then
. $(brew --prefix)/etc/bash_completion
fi

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;



# qfc commands
# https://github.com/pindexis/qfc
[[ -s "$HOME/.qfc/bin/qfc.sh" ]] && source "$HOME/.qfc/bin/qfc.sh"

# aliases to start rstudio server over ssh tunnel
    # make sure to add `www-address=127.0.0.1 to `/etc/rstudio/rserver.conf` so it listens on localhost for connections
alias rstudio_server_viao="open http://localhost:8787 && ssh -N -L localhost:8787:localhost:8787 viao"
alias rstudio_server_hp_int="open http://localhost:8788 && ssh -N -L localhost:8788:localhost:8787 hp_int"


# alias for newer version of screen for mac osx
# add flags to use UTF-8 encoding [U], detach elsewhere and reattach here [d], and reattach if possible, otherwise start a new session
alias screen="/usr/local/bin/screen -U"
alias screen_="/usr/local/bin/screen -U -c ~/dotfiles/screenrc_simple"

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
alias s='open -a "Sublime Text 3"'

# alias to view names of all of my scripts
alias myscripts='ls ~/Documents/Biostatistics/_CB/shell_scripts/'

# nano alias for options
alias nano='/usr/local/Cellar/nano/2.5.3/bin/nano --tabstospaces --tabsize=4 --smooth --autoindent --nohelp --quickblank -$'

# notes alias
alias notes='cd ~/Dropbox/Cole/notes; ls'
# todo alias will open todo note for school and backup when i save a copy
alias todo='nano -B --backupdir="~Dropbox/Cole/notes/school/" ~/Dropbox/Cole/notes/school/todo'
# jrnl alias will open jrnl note for work and backup when i save a copy
alias jrnl='nano -B --backupdir="~Dropbox/Cole/notes/school/" ~/Dropbox/Cole/notes/school/jrnl'

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
alias 'cb'='cd ~/Documents/Biostatistics/_CB'
alias 'scripts'='cd ~/Documents/Biostatistics/_CB/shell_scripts'

# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH

# MacPorts Installer addition on 2013-08-05_at_01:23:05: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH

# add path for gdal
export PATH=$PATH:/Library/Frameworks/GDAL.framework/Versions/1.11/Programs

test -r /sw/bin/init.sh && . /sw/bin/init.sh

# setting path for my own shell scripts
export PATH=$PATH:/Users/cole/Documents/Biostatistics/_CB/shell_scripts

# setting path for pandoc
export PATH=$PATH:/Applications/RStudio.app/Contents/MacOS/pandoc/pandoc
