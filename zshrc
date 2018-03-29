# include local paths in PATH before system paths
export PATH="$HOME/bin:$HOME/local/bin:$HOME/.cabal/bin:/usr/local/bin:$PATH"

# Load zsh module config files
for config_file ($HOME/.zsh/**/*.zsh) source $config_file

export NVM_DIR="/home/toddmohney/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/vault vault
