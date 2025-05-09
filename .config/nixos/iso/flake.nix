{
  description = "Custom NixOS installation media";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  inputs.wrapper-manager = {
    url = "github:viperML/wrapper-manager";
  };

  outputs = {
    self,
    nixpkgs,
    wrapper-manager,
  }: {
    packages.x86_64-linux.default = self.nixosConfigurations.exampleIso.config.system.build.isoImage;
    nixosConfigurations = {
      exampleIso = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [./iso.nix];
      };
    };
  };
}
