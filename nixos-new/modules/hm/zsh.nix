{pkgs, lib, ...}: {
  # Shared shell configuration
  zsh.enable = true;
  zsh.autocd = false;
  zsh.cdpath = [ "~/.local/share/src" ];
  zsh.dirHashes = {
    code = "$HOME/.local/share/src";
    nixos-config = "$HOME/.local/share/src/nixos-config";
  };
  zsh.plugins = [
    {
      name = "powerlevel10k";
      src = pkgs.zsh-powerlevel10k;
      file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
    }
    {
      name = "powerlevel10k-config";
      src = lib.cleanSource ./config;
      file = "p10k.zsh";
    }
  ];
  zsh.initExtraFirst = ''
    if [[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
      . /nix/var/nix/   profiles/default/etc/profile.d/nix-daemon.sh
      . /nix/var/nix/pro  files/default/etc/profile.d/nix.sh
    fi  
    
    # Define variables for directories 
    export PATH=$HOME/.npm-packages/bin:$HOME/bin:$PATH
    export NVM_DIR="$HOME/.nvm"

    # Cypress
    # Skip installation
    export CYPRESS_INSTALL_BINARY=0

    # Run binary
    export CYPRESS_RUN_BINARY="$(command -v Cypress)"

    # Remove history data we don't want to see
    export HISTIGNORE="pwd:ls:cd"

    # Don't grep inside node_modules
    GREP_OPTIONS="--exclude-dir=\.node_modules"

    # Emacs is my editor
    export ALTERNATE_EDITOR=""
    export EDITOR="emacsclient -t"
    export VISUAL="emacsclient -c -a emacs"
    e() {
        emacsclient -t "$@"
    }

    # nix shortcuts
    shell() {
        nix-shell '<nixpkgs>' -A "$1"
    }

    # git shortcuts
    alias gm="git merge --rebase"
    alias gp="git pull origin"

    # pnpm is a javascript package manager
    alias pn=pnpm
    alias px=pnpx

    # bat makes cat pretty
    alias cat=bat

    # Use difftastic, syntax-aware diffing
    alias diff=difft

    # Always color ls and group directories
    alias ls='ls --color=auto'

    # Weather report in your terminal
    alias weather='curl "http://wttr.in"'

    # Reboot into my dual boot Windows partition
    alias windows='systemctl reboot --boot-loader-entry=auto-windows'
  '';
}
