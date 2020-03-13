source $HOME/.bashrc
for config_file in ~/shell-scripts/*.sh; do source $config_file; done

export PATH="$HOME/.local/bin:$HOME/.cabal/bin:/usr/local/bin:$PATH"
export TERM=xterm-256color
