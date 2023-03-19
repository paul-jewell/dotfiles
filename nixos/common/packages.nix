{ pkgs }:

with pkgs; [
  act # run github actions locally
  alacritty
  aspell
  aspellDicts.en
#  awscli2
  bash-completion
  bat # A cat(1) clone with syntax highlighting
  btop
  coreutils
  #  difftastic
  du-dust
  #  docker
  #  docker-compose
  emacs
  ledger

  firefox

  fd
  feh
  fzf

  gcc
  gh # github
  git-filter-repo
  glow # CLI markdown viewer
  gnupg
  google-cloud-sdk
  go
  gopls

  home-manager
  htop
  hunspell
  iftop
#  jetbrains-mono
#  jq

  keepassxc

  killall
  libfido2
  neofetch
  #  nodePackages.live-server
  #  nodePackages.npm
  #  nodejs
  ngrok
  openssh
  pandoc
  pcmanfm
  pinentry
  python39
  python39Packages.virtualenv
  ranger
  ripgrep

  slack
  sqlite
  ssm-session-manager-plugin
  termite
#  terraform
#  terraform-ls
  tflint
  tree
  tmux
  
  unrar
  unzip
  wget
  
  xdg-desktop-portal
  xdg-utils
  
  zip
  zsh-powerlevel10k
  meslo-lgs-nf # Meslo Nerd Font patch for powerlevel10
]
