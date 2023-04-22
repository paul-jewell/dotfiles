{
  lib,
  config,
  nixosConfig,
  pkgs,
  ...
}: {
  imports = [
    ./autostart.nix
    ./beets.nix
    ./desktop.nix
    ./dev.nix
    ./fish.nix
    ./kitty.nix
    ./mpd.nix
    ./neovim.nix
    ./pipewire
    ./ranger.nix
    ./polybar.nix
    ./dunst.nix
    ./texlive.nix
  ];

  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    bash-completion
    bat
    ledger

    #    feh

    gcc
    gh

    keepassxc
    neofetch
    pinentry
    pandoc

    xdg-desktop-portal
    xdg-utils

    dig
    ffmpeg
    file
    fluxcd
    libqalculate
    psmisc
    ripgrep
    tree
    unzip

    ark
    dolphin
    evince
    gnome.nautilus
    imv
    okular

    qpwgraph
    virt-manager

    element-desktop
    signal-desktop
    tdesktop

    inkscape
    gimp
    krita
    tenacity
    libreoffice

    evolution
  ];

  theme = {
    enable = true;
    palette = "mocha";
  };

  programs.mpv = {
    enable = true;
    config = {
      hwdec = "auto";
      hwdec-codecs = "vaapi";
      profile = "gpu-hq";
      video-sync = "display-resample";
      volume = 50;
    };
  };

  programs.firefox.enable = true;
  programs.browserpass.enable = true;

  #  services.syncthing.enable = true;

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      obs-gstreamer
      obs-vaapi
    ];
  };

  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };

  xsession.preferStatusNotifierItems = true; # needed for network-manager-applet
  services.network-manager-applet.enable =
    lib.mkDefault nixosConfig.networking.networkmanager.enable;
  services.blueman-applet.enable = lib.mkDefault nixosConfig.services.blueman.enable;

  programs.k9s.enable = true;
  xdg.configFile."k9s/skin.yml".source = let
    theme = pkgs.fetchFromGitHub {
      owner = "catppuccin";
      repo = "k9s";
      rev = "322598e19a4270298b08dc2765f74795e23a1615";
      sha256 = "GrRCOwCgM8BFhY8TzO3/WDTUnGtqkhvlDWE//ox2GxI=";
    };
  in "${theme}/dist/mocha.yml";

  programs.btop.enable = true;

  programs.htop = {
    enable = true;
    settings =
      {
        "delay" = 10;
        ".tree_view_always_by_pid" = 1;
        "tree_view" = 1;
      }
      // (with config.lib.htop;
        leftMeters [
          (bar "LeftCPUs2")
          (bar "Memory")
          (bar "Swap")
        ])
      // (with config.lib.htop;
        rightMeters [
          (bar "RightCPUs2")
          (text "Tasks")
          (text "LoadAverage")
          (text "Uptime")
        ]);
  };
}
