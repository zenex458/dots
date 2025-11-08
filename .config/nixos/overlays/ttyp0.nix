{inputs, ...}: {
  nixpkgs = {
    overlays = [
      (final: prev: {
        uw-ttyp0 = prev.uw-ttyp0.override {
          variantsDat = ''

            cat << EOF > VARIANTS.dat
            COPYTO AccStress PApostropheAscii
            COPYTO PAmComma AccGraveAscii
            COPYTO Digit0Slashed Digit0
            COPYTO MTilde AccTildeAscii
            COPYTO Space SpaceNoBreak
            EOF
          '';
        };
      })
    ];
  };
}
