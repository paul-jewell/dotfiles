{
  description = "System configuration flake - Paul Jewell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    #    nixos-hardware.url = "github:NixOS/nixos-hardware";
    # My fork - for development of x390 hardware files
    nixos-hardware.url = "github:paul-jewell/nixos-hardware/lenovo-x390";
    # pre-commit-hooks = {
    #   url = "github:cachix/pre-commit-hooks.nix";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nixos-hardware,
#     pre-commit-hooks,
    home-manager,
    agenix,
    ...
  }@inputs:
    let
      inherit (self) outputs;
      forEachSystem = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" ];
      forEachPkgs = f: forEachSystem (sys: f nixpkgs.legacyPackages.${sys});

      mkNixos = modules: nixpkgs.lib.nixosSystem {
        inherit modules;
        specialArgs = { inherit inputs outputs; };
      };

      mkHome = modules: pkgs: home-manager.lib.homeManagerConfiguration {
        inherit modules pkgs;
        extraSpecialArgs = { inherit inputs outputs; };
      };
    in
      {
        nixosModules = import ./modules/nixos;
        homeManagerModules = import ./modules/home-manager;

        overlays = import ./overlays { inherit inputs outputs; };
        
        devShells = forEachPkgs (pkgs: import ./shell.nix { inherit pkgs; });
        formatter = forEachPkgs (pkgs: pkgs.nixpkgs-fmt);

        nixosConfigurations = {
          # laptops
          isolde = mkNixos [ ./hosts/isolde ];
        };

        homeConfigurations = {
          "paul@isolde" = mkHome [./home/paul/isolde.nix] nixpkgs.legacyPackages."x86_64-linux";
        };
      };
}
  
        
        
          
    
#     flake-utils.lib.eachDefaultSystem (system: let
#       pkgs = nixpkgs.legacyPackages.${system};
#     in {
#       checks = {
#         pre-commit-check = pre-commit-hooks.lib.${system}.run {
#           src = ./.;
#           hooks = {alejandra.enable = true;};
#         };
#       };
#       devShells.default = pkgs.mkShell {
#         inherit (self.checks.${system}.pre-commit-check) shellHook;
#         packages = with pkgs; [
#           alejandra
#           agenix.packages.${system}.agenix
#         ];
#       };
#     })
#     // (let
#       system = "x86_64-linux";
#       pkgs = import nixpkgs {
#         inherit system;
#         config = {allowUnfree = true;};
#         overlays = [self.overlays.default];
#       };

#       username = "paul";

#       mkHost = {
#         hostName,
#         system,
#         pkgs,
#         modules,
#       }: {
#         ${hostName} = nixpkgs.lib.nixosSystem {
#           inherit system;
#           inherit pkgs;
#           modules =
#             [
#               home-manager.nixosModules.home-manager
#               {
#                 home-manager.useGlobalPkgs = true;
#                 home-manager.useUserPackages = true;
#                 home-manager.sharedModules = pkgs.lib.attrValues self.hmModules;
#               }
#               agenix.nixosModules.age
#               ./hosts/common
#               ./hosts/${hostName}
#               ({lib, ...}: {networking.hostName = lib.mkDefault hostName;})

#               (import ./home username)
#             ]
#             ++ modules;
#         };
#       };
#     in {
#       nixosConfigurations =
#         (mkHost {
#           inherit system;
#           inherit pkgs;
#           hostName = "isolde";
#           modules = [nixos-hardware.nixosModules.lenovo-thinkpad-t14-amd-gen3];
#         })
#         // (mkHost {
#           inherit system;
#           inherit pkgs;
#           hostName = "gandalf";
#           # modules = [];
#           modules = [nixos-hardware.nixosModules.lenovo-thinkpad-x390];
#         });
#     })
#     // {
#       hmModules = {
#         beets = import ./modules/hm/beets.nix;
#         catppuccin = import ./modules/hm/catppuccin.nix;
#         fish-theme = import ./modules/hm/fish-theme.nix;
#         fuzzel = import ./modules/hm/fuzzel.nix;
#         pipewire = import ./modules/hm/pipewire.nix;
#       };
#       overlays.default = import ./pkgs;
#     };
# }
