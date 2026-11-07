{
  nixpkgs = {
    overlays = [
      (final: prev: { keepassxc = prev.keepassxc.override { withKeePassNetworking = false; }; })
    ];
  };
}
