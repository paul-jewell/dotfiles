{
  lib,
  pkgs,
  ...
}: {
  boot = {
    loader = {
      systemd-boot.enable = true;
      systemd-boot.configurationLimit = 10;
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot/efi";
    };

    plymouth = {
      enable = true;
      theme = "bgrt";
      font = "${pkgs.fira}/share/fonts/opentype/FiraSans-Regular.otf";
    };
  };
}

      
