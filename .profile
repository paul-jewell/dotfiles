# Define PATH
export PATH=$(find -L ~/.bin -type d -printf %p:)$PATH

# Define which gpg key to use for password-store
export PASSWORD_STORE_GPG_OPTS="--default-key 32089A9C90EAF4BB"
# .profile setup for gentoo systems

# Invoke GnuPG-Agent the first time we login.
# Does `~/.gpg-agent-info' exist and points to gpg-agent process accepting signals?
#if test -f "$HOME"/.gpg-agent-info && \
#    kill -0 "$(cut -d: -f 2 "$HOME"/.gpg-agent-info)" 2>/dev/null; then
#  GPG_AGENT_INFO=$(cut -c 16- < "$HOME"/.gpg-agent-info)
#else
#  # No, gpg-agent not available; start gpg-agent
#  eval "$(gpg-agent --daemon --no-grab $HOME/.gpg-agent-info)"
#fi

#GPG_TTY=$(tty)
#export GPG_TTY
#export GPG_AGENT_INFO

# Load .bashrc to get login environment
[ -f ~/.bashrc ] && . ~/.bashrc
. "$HOME/.cargo/env"
