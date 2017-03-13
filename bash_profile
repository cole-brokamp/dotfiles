export TERM=screen-256color

source ~/dotfiles/prompt.sh

# export secrets if it exists
source ~/dotfiles/secrets.sh 2> /dev/null

## pick between macOS and unix
if [ "$(uname)" == "Darwin" ]; then
    source ~/dotfiles/aliases_unix.sh
    source ~/dotfiles/aliases_macOS.sh
    # brew completion
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
        . $(brew --prefix)/etc/bash_completion
    fi
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
   # set path for linux brew
    export PATH="$HOME/.linuxbrew/bin:$PATH"
    export MANPATH="HOME/.linuxbrew/share/man:$MANPATH"
    export INFOPATH="HOME/.linuxbrew/share/info:$INFOPATH"
    source ~/dotfiles/aliases_unix.sh
fi


# Append to the Bash history file, rather than overwriting it
shopt -s histappend;

# Autocorrect typos in path names when using `cd`
shopt -s cdspell;

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;

# Solarized colorscheme for GNU `ls`:
eval `dircolors ~/dotfiles/dircolors-solarized`

# setting path for my own scripts
export PATH=$PATH:~/dotfiles/bin

# export COLUMNS variable for proper setting of R's width on resize
export COLUMNS

# set default editor
export VISUAL=vim
export EDITOR="$VISUAL"
