{
  pkgs,
  inputs,
  modulesPath,
  wrapper-manager,
  lib,
  config,
  ...
}: {
  imports = [(modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix") inputs.home-manager.nixosModules.home-manager];
  networking.wireless.enable = false;
  networking.networkmanager = {enable = true;};

  console = {
    useXkbConfig = true;
    font = "Lat2-Terminus16";
  };
  services = {
    xserver = {
      enable = true;
      displayManager.startx.enable = true;
      xkb = {
        layout = "gb";
        variant = "";
        options = "altwin:ctrl_alt_win";
      };
    };
  };

  home-manager.users.nixos = import ./home.nix;

  systemd = {
    services.sshd.wantedBy = pkgs.lib.mkForce ["multi-user.target"];
    targets = {
      sleep.enable = false;
      suspend.enable = false;
      hibernate.enable = false;
      hybrid-sleep.enable = false;
    };
  };

  environment.systemPackages = with pkgs; [
    tmux
    cryptsetup
    xterm
    git
    dmenu
    redshift
  ];
  isoImage.squashfsCompression = "gzip -Xcompression-level 1";
}
