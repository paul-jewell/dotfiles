{pkgs, ...}: {
  #programs.sway.enable = true;
  #security.pam.services.gtklock = {};

  services.xserver = {
    enable = true;

    # enable touchpad support
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

    windowManager.stumpwm = {
      enable = true;
    };
    # If I am reading this correctly, stumpwm is being run from
    # a local clone of the stumpwm repository...
    #displayManager.session = [
    #  {
    #    manage = "window";
    #    name = "stumpwm";
    #    start = ''
    #      export SBCL_HOME=${pkgs.sbcl}/lib/sbcl
    #      $HOME/projects/stumpwm/stumpwm
    #    '';
    #  }
    #];

    windowManager.bspwm = {
      enable = true;
      configFile = ./config/bspwmrc;
      sxhkd.configFile = ./config/sxhkdrc;
    };
    xautolock.enable = true;
  };

  environment.systemPackages = with pkgs; [
    rofi
    rofi-calc
    feh
  ];

  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      CPU_SCALING_GOVERNOR_ON_AC = "powersave";

      # The following prevents the battery from charging fully to preserve
      # lifetime. Run `tlp fullcharge` to temporarily force full charge.
      # https://linrunner.de/tlp/faq/battery.html#how-to-choose-good-battery-charge-thresholds
      # Based on the information regarding Lenovo laptops at this link,
      # I have chosen not to implement these limits.
      #START_CHARGE_THRESH_BAT0=40;
      #STOP_CHARGE_THRESH_BAT0=50;

      # 100 being the maximum - limit the speed of the CPU to reduce
      # heat and increase battery usage:
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MAX_PERF_ON_BAT = 75;
    };
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.avahi = {
    enable = true;
    nssmdns = true;
  };

  fonts = {
    fonts = with pkgs; [
      fira
      d2coding
      envypn-font
      font-awesome
      # feather font - clarify location / package
      # feather
      hack-font
      iosevka
      corefonts
      fira-code
      fira-code-symbols
      meslo-lgs-nf
      #roboto
      #roboto-mono
      nerdfonts
      #(nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
    ];

    enableDefaultFonts = true;

    fontDir = {
      enable = true;
      decompressFonts = true;
    };

    fontconfig = {
      enable = true;
      hinting.autohint = true;
      antialias = true;
    };
    enableGhostscriptFonts = true;

    # fontconfig = {
    #   cache32Bit = true;
    #   defaultFonts = {
    #     sansSerif = ["Fira Sans"];
    #     monospace = ["Fira Code"];
    #   };

    #   localConf = ''
    #     <?xml version="1.0"?>
    #     <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
    #     <fontconfig>
    #       <alias>
    #         <family>Fira Code</family>
    #         <prefer><family>Symbols Nerd Font</family></prefer>
    #       </alias>
    #       <alias>
    #         <family>Fira Code,Fira Code Medium</family>
    #         <prefer><family>Symbols Nerd Font</family></prefer>
    #       </alias>
    #       <alias>
    #         <family>Fira Code,Fira Code SemiBold</family>
    #         <prefer><family>Symbols Nerd Font</family></prefer>
    #       </alias>
    #       <alias>
    #         <family>Fira Code,Fira Code Light</family>
    #         <prefer><family>Symbols Nerd Font</family></prefer>
    #       </alias>
    #       <alias>
    #         <family>Fira Code,Fira Code Retina</family>
    #         <prefer><family>Symbols Nerd Font</family></prefer>
    #       </alias>
    #       <alias>
    #         <family>Monocraft</family>
    #         <prefer><family>Symbols Nerd Font</family></prefer>
    #       </alias>
    #     </fontconfig>
    #   '';
    # };
  };
}
