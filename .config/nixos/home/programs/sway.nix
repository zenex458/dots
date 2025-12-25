{pkgs, ...}: {
  services.swayidle = let
    lock = "${pkgs.swaylock}/bin/swaylock --daemonize";
    display = status: "${pkgs.niri}/bin/niri msg action power-${status}-monitors";
  in {
    enable = true;
    timeouts = [
      {
        timeout = 1800;
        command = lock;
      }
      {
        timeout = 2100;
        command = display "off";
        resumeCommand = display "on";
      }
    ];
    events = [
      {
        event = "before-sleep";
        command = (display "off") + "; " + lock;
      }
      {
        event = "after-resume";
        command = display "on";
      }
      {
        event = "lock";
        command = (display "off") + "; " + lock;
      }
      {
        event = "unlock";
        command = display "on";
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
