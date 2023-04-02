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
    consoleLogLevel = 0;
    kernelParams = ["quiet" "udev.log_level=2"];

    tmpOnTmpfs = true;
    tmpOnTmpfsSize = "75%";
  };
}

