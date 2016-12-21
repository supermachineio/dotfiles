export GNUPGHOME="$HOME/.gnupg"
export GPG_TTY=$(tty)
#
# start the agent
gpgconf --launch gpg-agent
