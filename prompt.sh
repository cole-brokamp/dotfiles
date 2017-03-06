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
# PS1="\[\033[36m\]\u\[\033[m\] @ \[\033[32m\]\h: \[\033[m\][\[\033[33;1m\]\w\[\033[m\]]\[\033[m\]\[\033[38;5;93m\] \$git_branch\$git_dirty\]\[\033[38;5;15m\]\\n  > "
BLACK=$(tput setaf 235)
GREY=$(tput setaf 240)
YELLOW=$(tput setaf 136)
ORANGE=$(tput setaf 166)
RED=$(tput setaf 160)
MAGENTA=$(tput setaf 125)
VIOLET=$(tput setaf 61)
BLUE=$(tput setaf 33)
CYAN=$(tput setaf 37)
GREEN=$(tput setaf 29)
											  
											  
PS1="\[\033[$BLUE\]\u\[\033[m\] @ \[\033[$GREEN\]\h: \[\033[m\][\[\033[$YELLOW\]\w\[\033[m\]]\[\033[m\]\[\033[$VIOLET\] \$git_branch\$git_dirty\]\[\033[38;5;15m\]\\n  > "
