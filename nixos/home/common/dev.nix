{
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [git-extras];

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

 #   ignore = [ "*.swp" ];
    userName = "Paul Jewell";
    userEmail = "paul@teulu.org";
#     signing = {
#       signByDefault = true;
# #      key = "61BFB242EBCAEB67";
#     };

    delta = {
      enable = true;
      options.navigate = true;
    };

    extraConfig = {
      core = {
        autocrlf = "input";
        editor = "nvim";
      };
      color.ui = "auto";
      diff.colorMoved = "default";
      push.followTags = true;
      pull.rebase = true;
      init.defaultBranch = "main";
      url = {
        "https://github.com/".insteadOf = "github:";
        "ssh://git@github.com/".pushInsteadOf = "github:";
        "https://gitlab.com/".insteadOf = "gitlab:";
        "ssh://git@gitlab.com/".pushInsteadOf = "gitlab:";
        "https://git.sr.ht/".insteadOf = "srht:";
        "ssh://git@git.sr.ht/".pushInsteadOf = "srht:";
        "https://codeberg.org/".insteadOf = "codeberg:";
        "ssh://git@codeberg.org/".pushInsteadOf = "codeberg:";
      };

      # Replace the default set by programs.git.signing.signByDefault
      tag.gpgSign = false;
    };
  };


  programs.gpg = {
    enable = true;
    homedir = "${config.xdg.dataHome}/gnupg";
  };
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 1209600;
    defaultCacheTtlSsh = 1209600;
    maxCacheTtl = 1209600;
    maxCacheTtlSsh = 1209600;
    extraConfig = "allow-preset-passphrase";
  };

  services.gnome-keyring.enable = true;

  programs.password-store.enable = true;
}
