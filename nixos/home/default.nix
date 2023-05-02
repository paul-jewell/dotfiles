username: {
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (lib) optional;
in {
  users.mutableUsers = false;
  users.users."${username}" = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups =
      ["wheel" "input" "audio" "video"]
      ++ optional config.security.rtkit.enable "rtkit"
      ++ optional config.networking.networkmanager.enable "networkmanager"
      ++ optional config.programs.adb.enable "adbusers"
      ++ optional config.virtualisation.libvirtd.enable "libvirtd"
      ++ optional config.virtualisation.podman.enable "podman";
    hashedPassword = "$y$j9T$ZCuWqmlsKtA9OK3e4kg5H.$u6sqp.4En.XHL7H20MVVYPm2FIhAuP7/w4OtFEsfKq5";
  };

  age.secrets."beets-secrets.yaml" = {
    file = ../secrets/common/beets-secrets.yaml;
    owner = username;
  };
  # age.secrets."listenbrainz-token" = {
  #   file = ../secrets/common/listenbrainz-token;
  #   owner = username;
  # };

  nix.settings.trusted-users = [username];

  home-manager.users."${username}" = {
    imports = [./common ./${config.networking.hostName}];

    home.username = username;
    home.homeDirectory = "/home/${username}";

    home.stateVersion = config.system.stateVersion;

    programs.home-manager.enable = true;
    systemd.user.startServices = "sd-switch";

    nixpkgs.config.allowUnfree = true;
  };
}
