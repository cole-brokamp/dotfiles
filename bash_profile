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

# Append to the Bash history file, rather than overwriting it
shopt -s histappend;

# Autocorrect typos in path names when using `cd`
shopt -s cdspell;

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;

# aliases to start rstudio server over ssh tunnel
    # make sure to add `www-address=127.0.0.1` to `/etc/rstudio/rserver.conf` so it listens only on localhost for connections
    # use ssh port forwarding to make it available on local machine; this way nobody can login without ssh access
    # also make applications using fluid application, so rstudio is standalone window instead of in browser
alias rstudio_server_viao="open /Applications/RStudio\ Server\ \(viao\).app && ssh -f -N -L localhost:8787:localhost:8787 viao"
alias rstudio_server_hp_int="open /Applications/RStudio\ Server\ \(hp_int\).app && ssh -f -N -L localhost:8788:localhost:8787 hp_int"

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"

# Print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'

# sshuttle aliases
alias sshuttle_viao="sshuttle --dns -r viao 0/0"

# python quick server alias
    # will make current directory available on local network, port 8000
alias quick_serve="python -m SimpleHTTPServer"

# alias to lock mac
alias lock='/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'

# alias to cd to current finder window
alias cdf='cd "`osascript -e "tell application \\"Finder\\" to get POSIX path of (insertion location as alias)"`"'

# alias to copy working directory
alias cwd="pwd | tr -d '\r\n' | pbcopy | echo '=> working directory copied to keyboard'"

# alias to open file in new R Studio instance
alias rstudio='open -na "rstudio"'

# alias to open file in Sublime
alias s='open -a "Submlime"'

# alias for googler
alias g='googler --colors bjdxxy -n 6'

# ip address finder aliases
alias ip_public='wget http://ipinfo.io/ip -qO -'
alias ip_local='ipconfig getifaddr en0'

# nano alias for options
alias nano='/usr/local/Cellar/nano/2.7.4/bin/nano --tabstospaces --tabsize=4 --smooth --autoindent --nohelp --quickblank -$'

# notes alias
alias notes='cd /Users/cole/Library/Mobile\ Documents/com~apple~CloudDocs/notes; ls'
# todo alias will open todo note for school and backup when i save a copy
alias todo='nano -B --backupdir="/Users/cole/Library/Mobile\ Documents/com~apple~CloudDocs/notes/cchmc/" /Users/cole/Library/Mobile\ Documents/com~apple~CloudDocs/notes/cchmc/todo'

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
alias 'biostat'='cd ~/Biostatistics'
alias 'tmp'='cd ~/Biostatistics/_TMP'
alias 'cb'='cd ~/Biostatistics/_CB'

# setting path for my own scripts
export PATH=$PATH:/Users/cole/dotfiles/bin

# homebrew bash completion
    # install additional scripts at https://github.com/Homebrew/homebrew-completions
if [ -f $(brew --prefix)/etc/bash_completion ]; then
. $(brew --prefix)/etc/bash_completion
fi
