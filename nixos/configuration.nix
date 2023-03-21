# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, 
  pkgs,
  lib,
  settings,
  ... 
}:
 let
   user = "paul";
 in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./common
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 10;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = "isolde"; # Define your hostname.

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/London";

# Manages keys and such
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Needed for anything GTK related
  programs.dconf.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_GB.UTF-8";
    LC_IDENTIFICATION = "en_GB.UTF-8";
    LC_MEASUREMENT = "en_GB.UTF-8";
    LC_MONETARY = "en_GB.UTF-8";
    LC_NAME = "en_GB.UTF-8";
    LC_NUMERIC = "en_GB.UTF-8";
    LC_PAPER = "en_GB.UTF-8";
    LC_TELEPHONE = "en_GB.UTF-8";
    LC_TIME = "en_GB.UTF-8";
  };

  # Enable the X11 windowing system.
  #services.xserver.enable = true;

  # xserver setup
  services.xserver = {
    enable = true;

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;

    # Configure keymap in X11
    layout = "gb";
    xkbModel = "pc105";
    xkbOptions = "ctrl:nocaps";
    xkbVariant = "extd";

    # Window management
    displayManager.defaultSession = "none+bspwm";
    displayManager.lightdm = {
      enable = true;
      greeters.slick.enable = true;
      background = ./config/login-wallpaper.png;
    };
    desktopManager.xfce.enable = true;
    
    # windowManager.stumpwm.enable = true;
    windowManager.bspwm = {
      enable = true;
      configFile = ./config/bspwmrc;
      sxhkd.configFile = ./config/sxhkdrc;
    };
    windowManager.i3 = {
      enable = true;
    };
    xautolock.enable = true;
  };

  # Better support for general peripherals
  boot.kernelModules = [ "uinput" ];
  
  # Configure console keymap
  console.keyMap = "uk";
  
  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.brlaser ];

  services.tlp.enable = true;
  
  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${user} = {
    isNormalUser = true;
    description = "Paul Jewell";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  fonts = {
    fontconfig.enable = true;
	  fontconfig.hinting.autohint = true;
	  fontconfig.antialias = true;
	  fontDir.enable = true;
	  enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      d2coding
      envypn-font
      font-awesome
      hack-font
      fira-code
      fira-code-symbols
      iosevka
      nerdfonts
      meslo-lgs-nf
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    neovim-unwrapped # Do not forget to add an editor.
    wget
    gcc
    rustup
    nodejs
    rofi
    rofi-calc

    alacritty
    screenkey
    tree
    tdesktop
    vlc

    gitAndTools.gitFull
    inetutils
    
    # security
    yubikey-manager
    yubikey-manager-qt
    yubikey-agent
    keepassxc
    pinentry-curses
    
    # For file system events
    inotify-tools
    libnotify

    # Wallpapers TODO: Nitrogen better?
    feh

    fontconfig
    font-manager

    # screen shot tools
    flameshot
    simplescreenrecorder

    gnumake
    gnugrep

    brlaser
    bc
    betterlockscreen
    yad
    
    # Applications
    gimp
    frescobaldi

    procps

    i3bar
  ];

  
  services.picom = {
    enable = true;
    settings = {
      animations = true;
      animation-stiffness = 300.0;
      animation-dampening = 35.0;
      animation-clamping = false;
      animation-mass = 1;
      animation-for-workspace-switch-in = "auto";
      animation-for-workspace-switch-out = "auto";
      animation-for-open-window = "slide-down";
      animation-for-menu-window = "none";
      animation-for-transient-window = "slide-down";
      corner-radius = 12;
      rounded-corners-exclude = [
        "class_i = 'polybar'"
        "class_g = 'i3lock'"
      ];
      round-borders = 3;
      round-borders-exclude = [];
      round-borders-rule = [];
      shadow = true;
      shadow-radius = 8;
      shadow-opacity = 0.4;
      shadow-offset-x = -8;
      shadow-offset-y = -8;
      fading = false;
      inactive-opacity = 0.8;
      frame-opacity = 0.7;
      inactive-opacity-override = false;
      active-opacity = 1.0;
      focus-exclude = [
      ];
      
      opacity-rule = [
        "100:class_g = 'i3lock'"
        "60:class_g = 'Dunst'"
        "100:class_g = 'Alacritty' && focused"
        "90:class_g = 'Alacritty' && !focused"
      ];
      
      blur-kern = "3x3box";
      blur = {
        method = "kernel";
        strength = 8;
        background = false;
        background-frame = false;
        background-fixed = false;
        kern = "3x3box";
      };
      
      shadow-exclude = [
        "class_g = 'Dunst'"
      ];
      
      blur-background-exclude = [
        "class_g = 'Dunst'"
      ];
      
      backend = "glx";
      vsync = false;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      detect-rounded-corners = true;
      detect-client-opacity = false;
      detect-transient = true;
      detect-client-leader = true;
      use-damage = true;
      log-level = "info";
      
      wintypes = {
        normal = { fade = true; shadow = false; };
        tooltip = { fade = true; shadow = false; opacity = 0.75; focus = true; full-shadow = false; };
        dock = { shadow = false; };
        dnd = { shadow = false; };
        popup_menu = { opacity = 1.0; };
        dropdown_menu = { opacity = 1.0; };
      };
    };
  };

  # TODO:: My editor runs as a daemon
  services.emacs = {
    enable = true;
    defaultEditor = true;
    # package = pkgs.emacsWithPackagesFromUsePackage {
    #   config = "~/dotfiles/.emacs.d/";
    #   package = pkgs.emacsUnstable;
    #   alwaysEnsure = true;
    # };
  };

  xdg.portal.xdgOpenUsePortal = true;
  
  environment.sessionVariables = rec {
    XDG_CACHE_HOME  = "\${HOME}/.cache";
    XDG_CONFIG_HOME = "\${HOME}/.config";
    XDG_BIN_HOME    = "\${HOME}/.local/bin";
    XDG_DATA_HOME   = "\${HOME}/.local/share";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.gvfs.enable = true; # Mount, trash, and other functionalities
  services.tumbler.enable = true; # Thumbnail support for images
  
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
  };
}
