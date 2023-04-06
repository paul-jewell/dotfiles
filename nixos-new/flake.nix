{
  description = "Home Manager configuration of Jane Doe";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    # pre-commit-hooks = {
    #   url = "github:cachix/pre-commit-hooks.nix";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    screenshot-bash = {
      url = "git+https://codeberg.org/Scrumplex/screenshot-bash";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    nixos-hardware,
#    pre-commit-hooks,
    home-manager,
    screenshot-bash,
    ...
  }:
    (let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = {allowUnfree = true;};
        overlays = [screenshot-bash.overlays.default self.overlays.default];
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

          modules =
            [
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
        })
        // (mkHost {
          inherit system;
          inherit pkgs;

          hostName = "gandalf";

          modules = [nixos-hardware.nixosModules.lenovo-thinkpad-x390];
        });
    })
    // {
      hmModules = {
        beets = import ./modules/hm/beets.nix;
        catppuccin = import ./modules/hm/catppuccin.nix;
        fish-theme = import ./modules/hm/fish-theme.nix;
        fuzzel = import ./modules/hm/fuzzel.nix;
        pipewire = import ./modules/hm/pipewire.nix;
      };
      overlays.default = import ./pkgs;
    };
}
