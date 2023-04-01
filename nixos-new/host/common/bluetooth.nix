{
  config,
  lib,
  ...
}: {
  services.blueman.enable = lib.MkDefault config.hardware.bluetooth.enable;
}
