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

find_git_dirty() {
  local status=$(git status --porcelain 2> /dev/null)
  if [[ "$status" != "" ]]; then
      git_dirty=" ${red}$white"
  else
    git_dirty=" "
  fi
}

find_git_branch() {
  # Based on: http://stackoverflow.com/a/13003854/170413
  local branch
  if branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null); then
    if [[ "$branch" == "HEAD" ]]; then
      branch='detached*'
    fi
    git_branch="$git_dirty $branch"
  else
    git_branch=""
  fi
}

PROMPT_COMMAND="find_git_dirty; find_git_branch; $PROMPT_COMMAND"

PROMPT_DIRTRIM=5

PS1="
$white$blueB \u $blue${greenB}\
$white$greenB \h $green${yellowB}\
$white$yellowB \w $yellow${purpleB}\
$white$purpleB\${git_branch} $(tput sgr0)${purple}\
$(tput sgr0)\n  > "

# unicode arrow: ❯
