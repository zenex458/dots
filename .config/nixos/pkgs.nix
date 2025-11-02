{inputs, ...}: {
  nixpkgs = {
    # Configure your nixpkgs instance
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
    overlays = [
      (final: prev: {
        unstable = import inputs.nixpkgs-unstable {
          inherit prev;
          system = prev.system;
          config.allowUnfree = true;
        };
        #options are here: https://github.com/NixOS/nixpkgs/blob/c7ab75210cb8cb16ddd8f290755d9558edde7ee1/pkgs/by-name/ke/keepassxc/package.nix#L26
        keepassxc = prev.keepassxc.override {
          withKeePassBrowserPasskeys = false;
          withKeePassBrowser = false;
          withKeePassNetworking = false;
          withKeePassKeeShare = false;
          withKeePassSSHAgent = false;
        };

        inherit
          (prev.lixPackageSets.stable)
          nixpkgs-review
          nix-eval-jobs
          nix-fast-build
          colmena
          ;
      })
      inputs.niri.overlays.niri
    ];
  };
}
