{config, ...}: {
  services = {
    # services.opensnitch-ui.enable = true;
    ssh-agent.enable = true;
    emacs = {
      enable = true;
      startWithUserSession = true;
      client = {
        enable = true;
        arguments = [
          "-c"
          "-a"
          "emacs"
        ];
      };
    };

    mpd = {
      enable = true;
      musicDirectory = "${config.home.homeDirectory}/Music/Alt";
      dataDir = "${config.home.homeDirectory}/Music";
      dbFile = "${config.home.homeDirectory}/Music/mpd.db";
      network.startWhenNeeded = true;
      extraConfig = ''
        audio_output {
           	type "pipewire"
            	name "pipewire"
        }
      '';
    };

    dunst = {
      enable = true;
      settings = {
        global = {
          width = 300;
          height = 300;
          offset = "0x0";
          origin = "top-right";
          transparency = 0;
          frame_color = "#bdae93"; # c6c6c6
          font = "Ttyp0 Bold 10";
          vertical_alignment = "center";
          alignment = "center";
          mouse_left_click = "close_current";
          mouse_middle_click = "do_action, close_current";
          mouse_right_click = "close_all";
          notification_limit = 0;
          follow = "mouse";
        };
        urgency_low = {
          background = "#111111";
          foreground = "#a08a64";
          timeout = 10;
        };

        urgency_normal = {
          background = "#060606";
          foreground = "#bdae93";
          timeout = 10;
        };

        urgency_critical = {
          background = "#900000";
          foreground = "#FFFFFF";
          frame_color = "#FF0000";
          timeout = 0;
        };
      };
    };
  };
}
