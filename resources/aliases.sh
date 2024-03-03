alias ls='ls -Fhgo --color --group-directories-first'
alias ll='ls -l'
alias la='ls -al'
alias grep='grep --color=auto'

alias reload="exec $SHELL -l"
alias path='echo -e ${PATH//:/\\n}'
alias quick_serve="python3 -m http.server"

alias ip_public='wget http://ipinfo.io/ip -qO -'
alias ip_local="ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*' | grep -v '127.0.0.1'"

alias R='R --quiet --no-save'
alias r='radian --quiet --no-save'

alias di='docker images | less -SX'
alias dp='docker ps -a | less -SX'
alias dc='docker system prune'
alias de='docker exec -ti $( docker ps -a -q -l) /bin/bash'
alias dt='docker run -ti --name ctop --rm -v /var/run/docker.sock:/var/run/docker.sock quay.io/vektorlab/ctop:latest'
alias docker_update_all='docker images | grep -v REPOSITORY | awk '\''{print $1 ":" $2}'\'' | xargs -L1 docker pull' 

# on mac, cd to current finder window
alias cdf='cd "`osascript -e "tell application \\"Finder\\" to get POSIX path of (insertion location as alias)"`"'
