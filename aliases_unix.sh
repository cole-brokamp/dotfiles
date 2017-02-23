### unix aliases

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"

# Print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'

# python quick server alias
    # will make current directory available on local network, port 8000
alias quick_serve="python -m SimpleHTTPServer"

# ip addresses
alias ip_public='wget http://ipinfo.io/ip -qO -'
alias ip_local="ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*' | grep -v '127.0.0.1'"

# nano
alias nano='nano --tabstospaces --tabsize=4 --smooth --autoindent --nohelp --quickblank -$'

# grep
alias grep='grep --color=auto'

# R
alias R='R --quiet --no-save'
alias r='R --quiet --no-save'

##  SSH tunnels
# aliases to start rstudio server over ssh tunnel
# make sure to add `www-address=127.0.0.1` to `/etc/rstudio/rserver.conf` so it listens only on localhost for connections
alias tunnel="echo -e '\n\nssh -fNL localhost:<local-port>:localhost:<remote-port> <ssh-client>"
# sshuttle aliases
alias sshuttle_viao="sshuttle --dns -r viao 0/0"
alias sshuttle_hp="sshuttle --dns -r hp 0/0"




## Docker
alias di='docker images | less -SX'
alias dp='docker ps -a | less -SX'
alias dc='docker_clean'
alias de='docker exec -ti $( docker ps -a -q -l) /bin/bash'
alias docker_update_all='docker images | grep -v REPOSITORY | awk '\''{print $1 ":" $2}'\'' | xargs -L1 docker pull' # updates all local images
