{
  config,
  pkgs,
  ...
}: {

  programs.ssh = {
    enable = true;

    controlMaster = "auto";
    controlPath = "~/.ssh/sockets/master-%r@%n:%p";
    controlPersist = "10m";

    matchBlocks = let
      idFile = "~/.ssh/id_ed25519";
    in {
#      "git.sr.ht" = {
#        user = "git";
#        identityFile = idFile;
#      };
      "github.com" = {
#        user = "git";
        identityFile = "~/.ssh/isolde-github";
      };
#      "codeberg.org" = {
#        user = "git";
#        identityFile = idFile;
#      };
      "orac" = {
        user = "paul";
        hostname = "orac";
        identityFile = "~/.ssh/orac_25519";
      };

      "orac-ip" = {
        user = "paul";
        hostname = "192.168.1.2";
        identityFile = "~/.ssh/orac_25519";
      };

      "tristan" = {
        user = "paul";
        hostname = "tristan";
        identityFile = "~/.ssh/tristan_ed25519";
      };

      "shingo" = {
        user = "paul";
        hostname = "shingo";
        identityFile = "~/.ssh/shingo_ed25519";
      };
    };
  };
}
