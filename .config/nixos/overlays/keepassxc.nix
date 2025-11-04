{inputs, ...}: {
  nixpkgs = {
    overlays = [
      (final: prev: {
        keepassxc = prev.keepassxc.override {
          withKeePassBrowserPasskeys = false;
          withKeePassBrowser = false;
          withKeePassNetworking = false;
          withKeePassKeeShare = false;
          withKeePassSSHAgent = false;
        };
      })
    ];
  };
}
