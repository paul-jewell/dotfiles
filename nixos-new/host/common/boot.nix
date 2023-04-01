{lib, ...}: {
  boot = {
    bootspec.enable = lib.mkDefault true;

    loader = {
      systemd-boot.enable = true;
      systemd-boot.consoleMode = "max";
      efi.canTouchEfiVariables = true;
      timeout = 0;
    };

    # Quieter boot
    initrd.verbose = false;
    initrd.systemd.enable = true;
    cnsoleLogLevel = 0;
    kernelParams = ["quiet" "udev.log_level=3"];

    tmpOnTmpfs = true;
    tmpOnTmpfsSize = "75%";
  };
}

