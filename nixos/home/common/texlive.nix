{ config,
  pkgs,
  ... }: {
  services.texlive = {
    enable = true;
  };
}

