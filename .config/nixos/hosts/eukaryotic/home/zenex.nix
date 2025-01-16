{
  pkgs,
  inputs,
  lib,
  ...
}: {
  imports = [
    inputs.impermanence.nixosModules.home-manager.impermanence
    inputs.stylix.homeManagerModules.stylix
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
      "Dev"
      ".emacs.d"
      ".mozilla"
      "Sync"
      ".config/vesktop"
      ".config/simplex"
      ".config/Signal"
      ".local/share/simplex"
    ];
    files = [".local/share/.bash_history"];
    allowOther = true;
  };

  config.stylix = {
    enable = true;
    image = ../../../../../screenshot.png;
    polarity = "dark";
    # cursor.name = "plan9";
    iconTheme = {
      enable = true;
      package =
        pkgs.papirus-icon-theme;
      dark = "Papirus-Dark";
      light = "Papirus-Dark";
    };
    cursor = {
      package = pkgs.phinger-cursors;
      name = "phinger-cursors-dark";
    };
    fonts = {
      serif = {
        package = pkgs.uw-ttyp0;
        name = "Ttyp0";
      };

      sansSerif = {
        package = pkgs.uw-ttyp0;
        name = "Ttyp0";
      };

      monospace = {
        package = pkgs.uw-ttyp0;
        name = "Ttyp0";
      };

      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };
    targets.dunst.enable = false;
    targets.foot.enable = false;
    targets.emacs.enable = false;
    targets.hyprland.enable = false;
    targets.hyprpaper.enable = false;
    targets.hyprlock.enable = false;

    base16Scheme = {
      base00 = "000000";
      base01 = "2e2e2e";
      base02 = "414141";
      base03 = "555555";
      base04 = "a08a64";
      base05 = "aa9673";
      base06 = "b3a283";
      base07 = "bdae93";
      base08 = "B33929";
      base09 = "e67300";
      base0A = "c0c000";
      base0B = "75B329";
      base0C = "6cb2eb";
      base0D = "2874B2";
      base0E = "802caa";
      base0F = "aaaaff";
    };
  };
}
