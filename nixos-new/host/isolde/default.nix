{
  config,
  pkgs,
}: {
  imports = [./hardware-configuration.nix ./boot.nix ./nix.nix];

  hardware.enableRedistributableFirmware = true;
  # TODO: See if this is worth it!
  #  boot.kernelPackages = pkgs.linuxKernel.packages.linux_zen;

  powerManagement.cpuFreqGovernor = "powersave";
  services.fwupd.enable = true;

  networking.networkmanager.enable = true;

  hardware.bluetooth.enable = true;

  services.gnome.gnome-keyring.enable = true;
  security.pam.services = {
    login.gnupg = {
      enable = true;
      noAutostart = true;
      storeOnly = true;
    };
    gtklock.gnupg = config.security.pam.services.login.gnupg;
  };

  services.logind = {
    lidSwitch = "suspend-then-hibernate";
    extraConfig = ''
      HandlePowerKey=suspend-then-hibernate
      PowerKeyIgnoreInhibited=yes
      LidSwitchIgnoreInhibited=no
    '';
  };

  systemd.sleep.extraConfig = ''
    HibernateDelaySec=10m
  '';

  environment.systemPackages = with pkgs; [neovim];

  system.stateVersion = "23.05";
}
