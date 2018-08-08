# define colors

white=$(tput setaf 231)
grey=$(tput setaf 240)
greyB=$(tput setab 237)
blue=$(tput setaf 33)
blueB=$(tput setab 33)
green=$(tput setaf 29)
greenB=$(tput setab 29)
yellow=$(tput setaf 136)
yellowB=$(tput setab 136)
purple=$(tput setaf 61)
purpleB=$(tput setab 61)
red=$(tput setaf 160)

# change prompt colors for emacs multi-term
if [ -n "$INSIDE_EMACS" ]; then
    blue=$(tput setaf 4)
    green=$(tput setaf 2)
    yellow=$(tput setaf 3)
    red=$(tput setaf 1)
    purple=$(tput setaf 5)
fi

find_git_dirty() {
  local status=$(git status --porcelain 2> /dev/null)
  if [[ "$status" != "" ]]; then
      git_dirty="${red}*"
  else
    git_dirty=" "
  fi
}

find_git_branch() {
  # Based on: http://stackoverflow.com/a/13003854/170413
  local branch
  if branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null); then
    if [[ "$branch" == "HEAD" ]]; then
      branch='detached*'
    fi
    git_branch="$grey ❯$purple $branch $git_dirty"
  else
    git_branch=""
  fi
}

find_docker() {
    if grep docker /proc/1/cgroup -qsa; then
        dock="🐳 "
    else
        dock=""
    fi
}

PROMPT_COMMAND="find_docker; find_git_dirty; find_git_branch; $PROMPT_COMMAND"

PROMPT_DIRTRIM=5

PS1="
$blue \u\
$grey ❯$green \${dock}\h\
$grey ❯$yellow \w\
\${git_branch}\
$(tput sgr0)\n  > "

# PS1="
# $white$blueB \u $blue${greenB}\
# $white$greenB \${dock}\h $green${yellowB}\
# $white$yellowB \w $yellow${purpleB}\
# $white$purpleB\${git_branch} $(tput sgr0)${purple}\
# $(tput sgr0)\n  ❯ "

# PS1="\u @ \h in \w ❯ "


# unicode arrow: ❯
