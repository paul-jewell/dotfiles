{
  description = "Home Manager configuration of Jane Doe";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    #    nixos-hardware.url = "github:NixOS/nixos-hardware";
    # My fork - for development of x390 hardware files
    nixos-hardware.url = "github:paul-jewell/nixos-hardware/lenovo-x390";
    pre-commit-hooks = {
       url = "github:cachix/pre-commit-hooks.nix";
       inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
        url = "github:ryantm/agenix";
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
    flake-utils,
    nixos-hardware,
    pre-commit-hooks,
    home-manager,
    screenshot-bash,
    agenix,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {alejandra.enable = true; };
        };
      };
      devShells.default = pkgs.mkShell {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
        packages = with pkgs; [
          alejandra
          agenix.packages.${system}.agenix
        ];
      };
    })
    // (let
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
          modules = [
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
               home-manager.sharedModules = pkgs.lib.attrValues self.hmModules;
            }
            agenix.nixosModules.age
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
	      # modules = [];
        modules = [nixos-hardware.nixosModules.lenovo-thinkpad-x390];
      });
  })
  // {
    hmModules = {
      beets      = import ./modules/hm/beets.nix;
      catppuccin = import ./modules/hm/catppuccin.nix;
      fish-theme = import ./modules/hm/fish-theme.nix;
      fuzzel     = import ./modules/hm/fuzzel.nix;
      pipewire   = import ./modules/hm/pipewire.nix;
    };
    overlays.default = import ./pkgs;
  };
}
