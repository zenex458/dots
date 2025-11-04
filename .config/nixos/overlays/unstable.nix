{inputs, ...}: {
  nixpkgs = {
    overlays = [
      (final: prev: {
        unstable = import inputs.nixpkgs-unstable {
          inherit prev;
          system = prev.system;
          config.allowUnfree = true;
        };
      })
    ];
  };
}
