{
  pkgs,
  inputs,
  lib,
  ...
}: {
  imports = [
    inputs.impermanence.nixosModules.home-manager.impermanence
    ./firefox.nix
    ./hyprland.nix
    ./home.nix
  ];
  config.home.stateVersion = "24.05";
  config.home.username = "zenex";
  config.home.file = {
    ".local/bin" = {
      source = ./scripts;
      recursive = true;
      executable = true;
    };
  };
  config.home.persistence."/persistent/home/zenex" = {
    directories = [
      "Downloads"
      "Documents"
      ".emacs.d"
      ".mozilla"
      ".elfeed"
    ];
    files = [".local/share/.bash_history"];
    allowOther = true;
  };
}
