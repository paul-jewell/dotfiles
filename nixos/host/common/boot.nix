{lib, ...}: {
  boot = {
    # Needs unstable nixpkgs!
    bootspec.enable = lib.mkDefault true;

    loader = {
      systemd-boot.enable = true;
      systemd-boot.consoleMode = "max";
      efi.canTouchEfiVariables = true;
      timeout = 5;
    };

    # quieter boot
#    initrd.verbose = false;
#    initrd.systemd.enable = true;
#    consoleLogLevel = 0;
#    kernelParams = ["quiet" "udev.log_level=3"];

    tmp = {
      useTmpfs = true;
      tmpfsSize = "75%";
    };
  };
}
