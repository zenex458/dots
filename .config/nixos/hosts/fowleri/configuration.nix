{
  pkgs,
  inputs,
  lib,
  ...
}:
{
  imports = [
    ./disko-config.nix
    ./hardware-configuration.nix
    ../base.nix
  ];
  systemd = {
    targets = {
      sleep.enable = false;
      suspend.enable = false;
      hibernate.enable = false;
      hybrid-sleep.enable = false;
    };
  };

  services.logind.settings.Login.HandleLidSwitch = "ignore";

  networking = {
    firewall = {
      allowedUDPPorts = [ 9 ];
    };
    hostName = "fowleri";
    interfaces = {
      enp0s31f6 = {
        wakeOnLan.enable = true;
      };
    };
  };
}
