{inputs, ...}: {
  nixpkgs = {
    overlays = [
      (final: prev: {
        uw-ttyp0 = prev.uw-ttyp0.override {
          variantsDat = ./variants.dat;
        };
      })
    ];
  };
}
