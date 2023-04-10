{ config,
  pkgs,
  lib,
  ...
}: {
  services.emacs = {
    enable = true;
    defaultEditor = true;
    # package = pkgs.emacsWithPackagesFromUsePackage {
    #   config = "~/dotfiles/.emacs.d/";
    #   package = pkgs.emacsUnstable;
    #   alwaysEnsure = true;
    # };
  };
}
  
