# -*- mode: sh; -*-

export BASH_SILENCE_DEPRECATION_WARNING=1

export TERM=screen-256color

source ~/dotfiles/prompt.sh

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
if [ -n "$MODULESHOME" ]; then
# load modules
    module load tmux/2.4
    module load vim
    # module load R/3.6.2
    # module load proj.4/4.9.1
    # module load gdal/2.1.2
    # module load geos/3.5.1
    # module load gcc/4.9.0
    # module load rstudio/0.98

    # use newer version of git that was installed from source
    export PATH="$HOME/git-2.12.2:$PATH"
fi

