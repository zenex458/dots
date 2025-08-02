{
  description = "Custom NixOS installation media";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    wrapper-manager = {
      url = "github:viperML/wrapper-manager";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    wrapper-manager,
    ...
  } @ inputs: {
    packages.x86_64-linux.default = self.nixosConfigurations.exampleIso.config.system.build.isoImage;
    nixosConfigurations = {
      exampleIso = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs;
        };
        system = "x86_64-linux";
        modules = [./iso.nix];
      };
    };
  };
}
