{
  lib,
  config,
  pkgs,
  ...
}: {
  programs.niri = {
    settings = {
      xwayland-satellite = {
        enable = true;
        path = lib.getExe pkgs.xwayland-satellite;
      };
      gestures.hot-corners.enable = false;
      screenshot-path = "~/Downloads/Images/ss/%Y-%m-%d--%H-%M-%S.png";
      hotkey-overlay.skip-at-startup = true;
      prefer-no-csd = true;
      workspaces."1" = {name = "browser";};
      workspaces."2" = {name = "emacs";};
      workspaces."3" = {name = "vid";};
      workspaces."4" = {name = "mu";};
      workspaces."5" = {name = "comms";};
      spawn-at-startup = [
        {
          command = ["${lib.getExe pkgs.dunst}"];
        }
        {
          command = ["batt.sh"];
        }
        {
          command = ["tailscale-health.sh"];
        }
        {
          command = ["${lib.getExe pkgs.wlsunset}" "-S" "07:00" "-s" "20:00" "-T" "4800" "-t" "2000"];
        }
        {
          command = ["wl-paste" "--watch" "cliphist" "store"];
        }
        {
          command = ["${lib.getExe pkgs.light}" "-S" "50"];
        }
        {
          command = ["${lib.getExe pkgs.cliphist}" "wipe"];
        }
        {
          command = ["${lib.getExe pkgs.swaybg}" "-i" "${config.home.homeDirectory}/Downloads/Images/Dlowsat.png"];
        }
      ];
      environment = {
        DISPLAY = ":0";
      };

      input = {
        keyboard = {
          repeat-delay = 300;
          repeat-rate = 50;
          xkb = {
            layout = "gb";
            options = "altwin:ctrl_alt_win,caps:shift_nocancel,caps:backspace";
          };
        };
        touchpad.dwt = true;
        warp-mouse-to-focus.enable = true;
        focus-follows-mouse.enable = true;
      };
      cursor = {
        theme = "plan9";
        hide-when-typing = true;
      };
      outputs = {
        "eDP-1" = {
          scale = 1;
          mode = {
            height = 1920;
            width = 1080;
          };
          position = {
            # x = 1280;
            x = 0;
            y = 0;
          };
          focus-at-startup = true;
        };
        "HDMI-A-1" = {
          scale = 1;
          mode = {
            height = 1280;
            width = 1024;
          };
          position = {
            # x = 0;
            x = 1920;
            y = 0;
          };
        };
      };
      animations.enable = false;
      layout = {
        default-column-width = {proportion = 0.5;};
        preset-column-widths = [
          {proportion = 1. / 4.;}
          {proportion = 1. / 2.;}
          {proportion = 2. / 3.;}
        ];
        border = {
          enable = true;
          width = 1;
          active.color = "#bdae93";
          inactive.color = "#000000";
          urgent.color = "#9b0000";
        };
        focus-ring.enable = false;
        gaps = 0;
      };
      layer-rules = [
        {
          matches = [
            {
              namespace = "^notifications$";
            }
          ];
          block-out-from = "screen-capture";
        }
      ];
      window-rules = [
        {
          matches = [
            {
              app-id = "^firefox$";
            }
          ];
          open-on-workspace = "browser";
        }
        {
          matches = [
            {
              app-id = "^emacs$";
            }
          ];
          open-on-workspace = "emacs";
        }
        {
          matches = [
            {
              title = "^ncmpcpp$";
            }
          ];
          open-on-workspace = "mu";
          open-fullscreen = true;
        }
        {
          matches = [
            {
              app-id = "^mpv$";
            }
          ];
          open-on-workspace = "vid";
        }
        {
          matches = [
            {
              app-id = "^signal$";
            }
          ];
          block-out-from = "screen-capture";
          open-on-workspace = "comms";
        }
        {
          matches = [
            {
              app-id = "^org.keepassxc.KeePassXC$";
            }
          ];
          block-out-from = "screen-capture";
          open-on-workspace = "comms";
        }
        {
          matches = [
            {
              app-id = "^chat-simplex-desktop-MainKt$";
            }
          ];
          block-out-from = "screen-capture";
          open-on-workspace = "comms";
        }
        {
          matches = [
            {
              app-id = "^vesktop$";
            }
          ];
          # block-out-from = "screen-capture";
          open-on-workspace = "comms";
        }
      ];

      binds = with config.lib.niri.actions; let
        sh = spawn "sh" "-c";
      in
        {
          "Mod+Tab".action = spawn "show.sh";
          # "Mod+Return".action = spawn "${lib.getExe pkgs.kitty}" "${lib.getExe pkgs.tmux}";
          "Mod+Return".action = spawn "${pkgs.foot}/bin/footclient" "${lib.getExe pkgs.tmux}";
          "Mod+P".action = sh ''com="$(${pkgs.bemenu}/bin/bemenu-run)"; niri msg action spawn -- "$com"''; # this opens new programs in its own namespace
          "Mod+Shift+Q".action = close-window;
          "Mod+H".action = focus-column-left;
          "Mod+J".action = focus-window-down;
          "Mod+K".action = focus-window-up;
          "Mod+L".action = focus-column-right;
          "Mod+Ctrl+H".action = move-column-left;
          "Mod+Ctrl+J".action = move-window-down;
          "Mod+Ctrl+K".action = move-window-up;
          "Mod+Ctrl+L".action = move-column-right;
          "Mod+Home".action = focus-column-first;
          "Mod+End".action = focus-column-last;
          "Mod+Ctrl+Home".action = move-column-to-first;
          "Mod+Ctrl+End".action = move-column-to-last;
          "Mod+W".action = focus-monitor-left;
          "Mod+E".action = focus-monitor-right;
          "Mod+Shift+W".action = move-column-to-monitor-left;
          "Mod+Shift+E".action = move-column-to-monitor-right;
          "Mod+U".action = spawn "${pkgs.emacs-pgtk}/bin/emacsclient" "-c" "-a" "emacs";
          "Mod+A".action = spawn "vol.sh";
          # "Mod+C".action = spawn "firejail" "${lib.getExe pkgs.firefox}";
          "Mod+C".action = spawn "${lib.getExe pkgs.firefox}";
          # "Mod+Shift+C".action = spawn "firejail" "${lib.getExe pkgs.firefox}" "-P" "work";
          "Mod+Shift+C".action = spawn "${lib.getExe pkgs.firefox}" "-P" "work";
          "Mod+Shift+O".action = spawn "${lib.getExe pkgs.mpc}" "next";
          "Mod+Shift+I".action = spawn "${lib.getExe pkgs.mpc}" "prev";
          "Mod+Shift+P".action = spawn "${lib.getExe pkgs.mpc}" "toggle";
          "Mod+Shift+Prior".action = spawn "${lib.getExe pkgs.light}" "-A" "2";
          "Mod+Shift+Next".action = spawn "${lib.getExe pkgs.light}" "-U" "2";
          "Mod+Shift+Home".action = spawn "light.sh";
          "Mod+M".action = spawn "Menu";
          "Mod+Y".action = spawn "clipshow.sh";
          "Mod+BracketLeft".action = consume-or-expel-window-left;
          "Mod+BracketRight".action = consume-or-expel-window-right;
          "Mod+Comma".action = consume-window-into-column;
          "Mod+Period".action = expel-window-from-column;
          "Mod+R".action = switch-preset-column-width;
          "Mod+Shift+R".action = switch-preset-window-height;
          "Mod+Ctrl+R".action = reset-window-height;
          "Mod+F".action = maximize-column;
          "Mod+Shift+F".action = fullscreen-window;
          "Mod+Ctrl+F".action = expand-column-to-available-width;
          "Mod+Minus".action = set-column-width "-1%";
          "Mod+Equal".action = set-column-width "+1%";
          "Mod+Shift+Minus".action = set-window-height "-1%";
          "Mod+Shift+Equal".action = set-window-height "+1%";
          "Mod+V".action = toggle-window-floating;
          "Mod+Shift+V".action = switch-focus-between-floating-and-tiling;
          "Mod+Shift+Z".action = quit;
          "Print".action.screenshot = {show-pointer = false;};
        }
        # These two functions return a AttrsList in this format keybind+$NUM = $NUM
        # `//' merges the AttrsLists together
        // (
          lib.genAttrs
          (map (n: "Mod+${toString n}") (lib.range 1 9))
          (key: {
            action = focus-workspace (lib.toInt (lib.removePrefix "Mod+" key));
          })
        )
        // (
          lib.genAttrs
          (map (n: "Mod+Shift+${toString n}") (lib.range 1 9))
          (key: {
            action.move-window-to-workspace = lib.toInt (lib.removePrefix "Mod+Shift+" key);
          })
        );
    };
  };
}
