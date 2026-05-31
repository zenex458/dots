{ pkgs, ... }:
{
  services.swayidle =
    let
      lockcmd = "${pkgs.swaylock}/bin/swaylock --daemonize";
      display = status: "${pkgs.niri}/bin/niri msg action power-${status}-monitors";
    in
    {
      enable = true;
      timeouts = [
        {
          timeout = 1800;
          command = lockcmd;
        }
        {
          timeout = 2100;
          command = display "off";
          resumeCommand = display "on";
        }
      ];
      events = {
        "before-sleep" = (display "off") + "; " + lockcmd;
        "after-resume" = display "on";
        "lock" = (display "off") + "; " + lockcmd;
        "unlock" = display "on";
      };
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
