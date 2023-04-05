# Modular Configuration for all my systems

{
  description = "Home manager configuration for my systems";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
#    flake-utils.url = github:numtide/flake-utils;
    nixos-hardware.url = github:NixOS/nixos-hardware;

    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nix-serve-ng = {
    #   url = github:aristanetworks/nix-serve-ng;
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.utils.follows = "flake-utils";
    # };
        
    # TODO: Look into:
    # - github:ryantm:agenix - managing secrets with deployment
    # - github:nix-community/lanzaboote - Secure boot implementation

  };

  outputs = {
    self,
    nixpkgs,
#    flake-utils,
    nixos-hardware,
    home-manager,
#    nix-serve-ng,
    ...
  }: 
    (let     
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = {allowUnfree = true;};
#        overlays = [nix-serve-ng.overlays.default self.overlays.default];
      };
      username = "paul";

      mkHost = {
        hostName,
          system,
          pkgs,
          modules,
      }: {
        ${hostName} = nixpkgs.lib.nixosSystem {
          inherit system;
          inherit pkgs;

          modules = [
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              
              home-manager.sharedModules = pkgs.lib.attrValues self.hmModules;
            }
            
            ./host/common
            ./host/${hostName}
            ({lib, ...}: {networking.hostName = lib.mkDefault hostName;})
            
            (import ./home username)
          ]
          ++ modules;
        };
      };
    in {
      nixosConfigurations =
        (mkHost {
          inherit system;
          inherit pkgs;
          
          hostName = "isolde";
          
          modules = [nixos-hardware.nixosModules.lenovo-thinkpad-t14-amd-gen3];
        });
    })
    // {
      hmModules = {
        beets = import ./modules/hm/beets.nix;
        catppuccin = import ./modules/hm/catppuccin.nix;
        fish-theme = import ./modules/hm/fish-theme.nix;
        fuzzel = import ./modules/hm/fuzzel.nix;
        jellyfin-mpv-shim = import ./modules/hm/jellyfin-mpv-shim.nix;
        pipewire = import ./modules/hm/pipewire.nix;
      };
      overlays.default = import ./pkgs;
    };
}
