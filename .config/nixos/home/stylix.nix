{
  pkgs,
  inputs,
  config,
  ...
}: {
  imports = [
    inputs.stylix.homeModules.stylix
  ];

  stylix = {
    enable = true;
    autoEnable = false;
    # image = ./Dlowsat.png;
    # image = null;
    polarity = "dark";
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
        package = pkgs.iosevka-bin;
        name = "Iosevka Medium";
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
      gtk.enable = true;
      gnome.enable = true;
    };

    base16Scheme = {
      base00 = "060606";
      base01 = "2e2e2e";
      base02 = "414141";
      base03 = "515151";
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
