{pkgs, ...}: {
  services.swayidle = {
    enable = true;
    events = [
      {
        event = "before-sleep";
        command = "${pkgs.swaylock}/bin/swaylock -f";
      }
    ];
    timeouts = [
      {
        timeout = 1800;
        command = "${pkgs.swaylock}/bin/swaylock -f";
      }
      {
        timeout = 2100;
        command = "niri msg action power-off-monitors";
        resumeCommand = "niri msg action power-on-monitors";
      }
    ];
  };
  programs.swaylock = {
    enable = true;
    settings = {
      color = "060606";
      font-size = 24;
      indicator-idle-visible = false;
      indicator-radius = 100;
    };
  };
}
