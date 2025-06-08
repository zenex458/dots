{
  pkgs,
  inputs,
  config,
  lib,
  ...
}: {
  imports = [
    inputs.impermanence.nixosModules.home-manager.impermanence
    inputs.stylix.homeModules.stylix
    ./home.nix
  ];

  stylix = {
    enable = true;
    image = ./Dlowsat.png;
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
      size = 32;
    };
    fonts = {
      sizes = {
        applications = 10;
        terminal = 10;
      };
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
    targets = {
      firefox.profileNames = ["priv" "work"];
      dunst.enable = false;
      foot.enable = false;
      emacs.enable = false;
      hyprland.enable = false;
      hyprpaper.enable = false;
      hyprlock.enable = false;
      tmux.enable = false;
      neovim.enable = false;
      gtk.extraCss = ''
        // Remove rounded corners
        window.background { border-radius: 0; }
      '';
    };

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
