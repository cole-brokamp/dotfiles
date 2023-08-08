# -*- mode: sh; -*-

export DOCKER_BUILDKIT=1

export BASH_SILENCE_DEPRECATION_WARNING=1

export TERM=screen-256color

# export secrets if it exists
source ~/dotfiles/secrets.sh 2> /dev/null

## pick between macOS and unix
if [ "$(uname)" == "Darwin" ]; then
    source ~/dotfiles/aliases_unix.sh
    source ~/dotfiles/aliases_macOS.sh
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    source ~/dotfiles/aliases_unix.sh
fi


# Append to the Bash history file, rather than overwriting it
shopt -s histappend;

# Autocorrect typos in path names when using `cd`
shopt -s cdspell;

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;

# Solarized colorscheme for GNU `ls`:
eval `dircolors --sh ~/dotfiles/dircolors-solarized`

# setting path for my own scripts
export PATH=$PATH:~/dotfiles/bin

# add go path
export PATH=$PATH:/usr/local/go/bin

# export COLUMNS variable for proper setting of R's width on resize
export COLUMNS

# set default editor
export VISUAL=emacs
export EDITOR="$VISUAL"

# exec $SHELL -l # use my dotfiles
# source ~/.bash_profile

# bash completion
# [[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"

# function for vterm in emacs to get proper escape sequences
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# so emacs vterm can autoname buffers
export PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}:${PWD}\007"'

# so emacs vterm can autotrack directory and prompt
vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'

# do the following only if on the HPC
if command -v module &> /dev/null; then
# load modules
    module load tmux/2.4
    module load vim
fi

source ~/dotfiles/prompt.sh

if [ "$(uname)" == "Darwin" ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi
