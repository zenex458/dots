{
  description = "NixOS configuration";

  inputs = {
<<<<<<< HEAD
    nixpkgs.url = "nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
=======
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    #    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    # figureout how to add an unstable package https://github.com/Misterio77/nix-starter-configs/blob/main/standard/flake.nix
    # https://librephoenix.com/2024-02-10-using-both-stable-and-unstable-packages-on-nixos-at-the-same-time
    # Home manager
    home-manager.url = "github:nix-community/home-manager/release-24.11";
>>>>>>> main
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
<<<<<<< HEAD
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
=======
      home-manager,
      ...
    }@inputs:
    let
      inherit (self) outputs;
      # Supported systems for your flake packages, shell, etc.
      systems = [ "x86_64-linux" ];
    in
    {
      # NixOS configuration entrypoint
      # Available through 'nixos-rebuild --flake .#your-hostname'
      nixosConfigurations = {
        eukaryotic = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
>>>>>>> main
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
