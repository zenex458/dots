{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
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
        keepassxc = prev.keepassxc.override {
          withKeePassBrowserPasskeys = false;
          withKeePassBrowser = false;
          withKeePassNetworking = false;
          withKeePassKeeShare = false;
          withKeePassSSHAgent = false;
        };
      })
      inputs.niri.overlays.niri
    ];
  };
}
