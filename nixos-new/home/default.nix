username: {
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (lib) optional;
in {
  users.users."${username}" = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups =
      ["wheel" "input" "audio" "video"]
      ++ optional config.security.rtkit.enable "rtkit"
      ++ optional config.networking.networkmanager.enable "networkmanager"
      ++ optional config.programs.adb.enable "adbusers"
      ++ optional config.virtualisation.libvirtd.enable "libvirtd";
  };

  # age.secrets."beets-secrets.yaml" = {
  #   file = ../secrets/common/beets-secrets.yaml;
  #   owner = username;
  # };
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
