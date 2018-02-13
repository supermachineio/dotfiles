export GNUPGHOME="$HOME/.gnupg"
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"

gpgconf --launch gpg-agent
gpg-connect-agent updatestartuptty /bye >/dev/null

# Refresh gpg-agent tty in case user switches into an X session
# gpg-connect-agent updatestartuptty /bye >/dev/null

# start the agent
# gpgconf --launch gpg-agent
