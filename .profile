# Define PATH
export PATH=$(find -L  ~/.bin -type d -printf %p:)$PATH

if [ "$(hostname)" = "zeus" ] 
then
    echo Setting up profiles on guix system
    # Load the default Guix profile
    GUIX_PROFILE="$HOME/.guix-profile"
    . "$GUIX_PROFILE"/etc/profile

    # Load additional Guix profiles
    GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
    for i in "$GUIX_EXTRA_PROFILES"/*; do
        profile=$i/$(basename "$i")
        if [ -f "$profile"/etc/profile ]; then
            GUIX_PROFILE="$profile"
            . "$GUIX_PROFILE"/etc/profile
        fi
        unset profile
    done

    # don't use the system-wide PulseAudio configuration
    # ...need to understand why this is necessary...
    unset PULSE_CONFIG
    unset PULSE_CLIENTCONFIG

    # Export the path to IcedTea so the tools find it
    #export JAVA_HOME=$(dirname $(dirname $(readlink $(which java))))

    # Make sure we can reach the GPG agent for SSH auth
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

    # Make sure ls collates dotfiles first (for dired)
    export LC_COLLATE="C"

    # Many build scripts expect CC to contain the compiler command
    export CC="gcc"

    # Make applications in other profiles visible to launcher
    export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.guix-extra-profiles/music/music/share"
    export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.guix-extra-profiles/video/video/share"
    export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.guix-extra-profiles/browsers/browsers/share"

    # Ensure that font folders are loaded correctly
    xset +fp "$(dirname "$(readlink -f ~/.guix-extra-profiles/desktop/desktop/share/fonts/truetype/fonts.dir)")"

    export SUDO_ASKPASS="dmenupass"
    
    # emacs for editing system files...
    export VISUAL="emacs"
    export EDITOR="$VISUAL"
    export BROWSER="nyxt"
    export TERMINAL="st"
else 
    # .profile setup for gentoo systems

    # Invoke GnuPG-Agent the first time we login.
    # Does `~/.gpg-agent-info' exist and points to gpg-agent process accepting signals?
    if test -f "$HOME"/.gpg-agent-info && \
        kill -0 "$(cut -d: -f 2 "$HOME"/.gpg-agent-info)" 2>/dev/null; then
        GPG_AGENT_INFO=$(cut -c 16- < "$HOME"/.gpg-agent-info)
    else
        # No, gpg-agent not available; start gpg-agent
        eval "$(gpg-agent --daemon --no-grab --write-env-file $HOME/.gpg-agent-info)"
    fi
    export GPG_TTY=$(tty)
    export GPG_AGENT_INFO
fi

# Load .bashrc to get login environment
[ -f ~/.bashrc ] && . ~/.bashrc
