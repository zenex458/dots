{ inputs, ... }:
{
  nixpkgs = {
    overlays = [
      (final: prev: {
        unstable = import inputs.nixpkgs-unstable {
          localSystem = final.stdenv.hostPlatform;
          config.allowUnfree = true;
        };
      })
    ];
  };
}
