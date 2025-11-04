{pkgs, ...}: {
  gtk = {
    enable = true;
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome-themes-extra;
    };
    font = {
      name = "Iosevka";
      size = 10;
    };
  };
  qt = {
    enable = true;
    style.name = "adwaita-dark";
  };
}
