{
  description = "My flake";

  # Inputs
  # https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html#flake-inputs

  # It is also possible to "inherit" an input from another input. This is useful to minimize
  # flake dependencies. For example, the following sets the nixpkgs input of the top-level flake
  # to be equal to the nixpkgs input of the nixops input of the top-level flake:

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lanzaboote = {
      url = "github:nix-community/lanzaboote/v0.4.2";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence.url = "github:nix-community/impermanence";
  };

  # For more information about well-known outputs checked by `nix flake check`:
  # https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake-check.html#evaluation-checks

  outputs = inputs @ {nixpkgs, ...}: let
    # Used with `nixos-rebuild --flake .#<hosts>`
    # nixosConfigurations."<hosts>".config.system.build.toplevel must be a derivation
    hosts = ["nidus" "tetatnus"];
    mkSystem = hostname:
      nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs;
        };
        system = "x86_64-linux";
        modules = [./hosts/${hostname}/configuration.nix];
      };
  in {
    nixosConfigurations = nixpkgs.lib.genAttrs hosts mkSystem;
  };
}
