alias ls='ls -F --color=auto'
alias ll='ls -AFhl'

alias gitlog="git log -n 10"
alias vim-install-plugins="vim +PluginInstall! +qall"
alias zshconfig="vim ~/.zshrc"

alias workdisplay="xrandr --output DP-3 --mode 2560x1440"

alias blockdisplay="xrandr --output DP1 --mode 2560x1440"
alias blockdisplay-off="xrandr --output DP1 --off"

alias laptopdisplay="xrandr --output eDP-1 --mode 1920x1080"
alias laptopdisplay-off="xrandr --output eDP-1 --off"

alias switch-to-work-monitor="xrandr --output DP1 --mode 2560x1440 --output eDP1 --off"
alias switch-to-laptop-monitor="xrandr --output eDP1 --mode 1920x1080 --output DP1 --off"

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
alias dc-logs="docker-compose logs -f"

alias news="newsbeuter -C $HOME/.newsbeuter/config -u $HOME/.newsbeuter/urls"

alias byebye="systemctl suspend"
