{inputs, ...}: {
  nixpkgs = {
    overlays = [
      (final: prev: {
        iosevka = prev.iosevka.override {
          set = "";
          privateBuildPlan = ''
            [buildPlans.Iosevka]
            family = "Iosevka"
            spacing = "normal"
            serifs = "sans"
            noCvSs = true
            exportGlyphNames = false

            [buildPlans.Iosevka.variants.design]
            one = "base"
            seven = "straight-serifless-crossbar"
            r = "corner-hooked-serifed"
            lower-lambda = "straight-turn"
            brace = "curly"
            percent = "rings-continuous-slash-also-connected"

            [buildPlans.Iosevka.weights.Regular]
            shape = 400
            menu = 400
            css = 400

            [buildPlans.Iosevka.weights.Bold]
            shape = 700
            menu = 700
            css = 700

            [buildPlans.Iosevka.slopes.Upright]
            angle = 0
            shape = "upright"
            menu = "upright"
            css = "normal"

            [buildPlans.Iosevka.slopes.Italic]
            angle = 9.4
            shape = "italic"
            menu = "italic"
            css = "italic"
          '';
        };
      })
    ];
  };
}
