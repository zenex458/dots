{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      home-manager,
      ...
    }:
    let
      system = "x86_64-linux";
      overlay-unstable = final: prev: {
        unstable = import nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
        };

      };
    in
    {
      nixosConfigurations = {
        eukaryotic = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            (
              { config, pkgs, ... }:
              {
                nixpkgs.overlays = [ overlay-unstable ];
              }
            )
            ./nixos/configuration.nix
          ];
        };
      };
    };
}
