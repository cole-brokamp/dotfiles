source ~/dotfiles/prompt.sh

## pick between macOS and unix
if [ "$(uname)" == "Darwin" ]; then
    source ~/dotfiles/aliases_macOS.sh
    source ~/dotfiles/aliases_unix.sh
    # brew completion
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
        . $(brew --prefix)/etc/bash_completion
    fi
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    source ~/dotfiles/aliases_unix.sh
fi


# Append to the Bash history file, rather than overwriting it
shopt -s histappend;

# Autocorrect typos in path names when using `cd`
shopt -s cdspell;

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;


# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU
    colorflag="--color"
else # OS X
    colorflag="-G"
fi

# ls alias
alias ls='ls -Fh ${colorflag}'
alias ll='ls -Fhl ${colorflag}'
alias la='ls -Fhal ${colorflag}'
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced




# setting path for my own scripts
export PATH=$PATH:/Users/cole/dotfiles/bin

# export COLUMNS variable for proper setting of R's width on resize
export COLUMNS
