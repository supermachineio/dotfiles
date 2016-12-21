alias ls='ls -F --color=auto'
alias ll='ls -AFhl'

alias gitlog="git log -n 10"
alias vim-install-plugins="vim +PluginInstall! +qall"
alias zshconfig="vim ~/.zshrc"

alias workdisplay="xrandr --output DP-3 --mode 2560x1440"
alias laptopdisplay="xrandr --output eDP-1 --mode 1920x1080"

# usage: `sshkeygen email@address.com"
alias sshkeygen="ssh-keygen -t rsa -b 4096 -C"
# usage: `tmuxnew my-session-name`
alias tmuxnew="tmux new-session -s"
# usage: `untar /path/to/tarball`
alias untar="tar -xvf"


alias blackbox_register_all_env_files="find . -type f -name '*.env' -print0 2>/dev/null | xargs -0 blackbox_register_new_file"

alias dc-mongo="docker-compose exec tokumx mongo code_climate_development"
alias dc-ps="docker-compose ps"
alias dc-up="docker-compose up -d"
alias dc-down="docker-compose down"
alias dc-down="docker-compose down"
alias dc-reset-all="docker-compose down && docker volume rm harness_{kafka,zookeeper,redis,repos,tokumx}"
alias dc-rails-c="docker-compose exec app bundle exec rails c"
alias dc-logs="docker-compose logs -f"
alias dc-app-rspec="docker-compose exec app bundle exec rspec"
