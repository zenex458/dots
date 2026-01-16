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

  # isoImage.isoName = lib.mkForce "nixos.iso";
  networking = {
    wireless.enable = false;
    hostName = "nixiso";
    networkmanager = {enable = true;};
  };

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
  users.users.nixos.openssh.authorizedKeys.keys = [
  ];
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
    disko
    xclip
    htop
    lynx
    (writeScriptBin "install.sh" ../../../setup.sh)
  ];

  fonts = {
    fontconfig.enable = true;
    enableDefaultPackages = true;
    #packages = with pkgs; [
    #  noto-fonts
    #  noto-fonts-cjk-sans
    #  noto-fonts-emoji
    #  liberation_ttf
    #  fira-code
    #  fira-code-symbols
    #];
  };

  nix = {
    settings = {
      experimental-features = ["nix-command" "flakes"];
      download-buffer-size = 524288000;
    };
  };

  isoImage.squashfsCompression = "gzip -Xcompression-level 1";
}
