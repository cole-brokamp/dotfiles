# shell prompt name
PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]$(scutil --get ComputerName):\[\033[33;1m\]\W\[\033[m\]\$ "

# alias for newer version of screen for mac osx
alias screen="/usr/local/bin/screen"

# alias to view names of all of my scripts
alias myscripts='ls ~/Documents/Biostatistics/_CB/shell_scripts/'

# nano alias for options
alias nano='nano --tabstospaces --tabsize=4 --autoindent --nohelp --quickblank'

# notes alias
alias notes='cd ~/Dropbox/Cole/notes; ls'
# todo alias will open todo note for school and backup when i save a copy
alias todo='nano -B --backupdir="~Dropbox/Cole/notes/school/" ~/Dropbox/Cole/notes/school/todo'

# ls alias
alias ls='ls -GFh'
alias ll='ls -GFhal'
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

# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH

# add path for pwd script
export PATH=/Users/cole/Dropbox/Cole/pwd.sh:${PATH}

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
