alias ll='ls -AFhl --color=auto'

alias gitlog="git log -n 10"
alias vim-install-plugins="vim +PluginInstall! +qall"
alias zshconfig="vim ~/.zshrc"

alias display-home="xrandr --output DP-1 --mode 2560x1440 --output eDP-1 --off"
alias display-work="xrandr --output DP-1 --mode 2560x1440 --output eDP-1 --off"
alias display-laptop="xrandr --output eDP-1 --mode 1920x1080 --dpi 128 --output DP-1 --off --output HDMI-1 --off"

# usage: `sshkeygen email@address.com"
alias sshkeygen="ssh-keygen -t rsa -b 4096 -C"
# usage: `tmuxnew my-session-name`
alias tmuxnew="tmux new-session -s"
# usage: `untar /path/to/tarball`
alias untar="tar -xvf"

alias blackbox_register_all_env_files="find . -type f -name '*.env' -print0 2>/dev/null | xargs -0 blackbox_register_new_file"

alias rake="noglob rake"

alias dc-ps="docker-compose ps"
alias dc-up="docker-compose up -d"
alias dc-down="docker-compose kill && docker-compose down --remove-orphans"
alias dc-logs="docker-compose logs -f --tail=250"

alias news="newsbeuter -C $HOME/.newsbeuter/config -u $HOME/.newsbeuter/urls"

alias byebye="systemctl suspend"
