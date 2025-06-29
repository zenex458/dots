{
  # wayland.windowManager.hyprland = {
  #   enable = true;
  #   settings = {
  #     input = {
  #       kb_layout = "gb";
  #       kb_options = "altwin:ctrl_alt_win,caps:shift_nocancel,caps:backspace";
  #       repeat_rate = 50;
  #       repeat_delay = 300;
  #       follow_mouse = 1;
  #       sensitivity = 0;
  #       touchpad.natural_scroll = "no";
  #     };
  #     general = {
  #       gaps_in = 0;
  #       gaps_out = 0;
  #       border_size = 1;
  #       "col.active_border" = "rgba(bdae93ff)";
  #       "col.inactive_border" = "rgba(000000aa)";
  #       layout = "master";
  #       allow_tearing = false;
  #     };
  #     cursor = {
  #       hide_on_key_press = false;
  #       inactive_timeout = 20;
  #     };
  #     decoration = {
  #       rounding = 0;
  #       blur = {
  #         enabled = false;
  #         size = 0;
  #         passes = 1;
  #       };
  #       shadow.enabled = false;
  #     };
  #     animations = {
  #       enabled = false;
  #       first_launch_animation = false;
  #     };
  #     gestures = {
  #       workspace_swipe = "off";
  #     };
  #     misc = {
  #       force_default_wallpaper = 0;
  #       disable_hyprland_logo = true;
  #       disable_splash_rendering = true;
  #     };
  #     binds = {
  #       window_direction_monitor_fallback = false;
  #       movefocus_cycles_fullscreen = true;
  #     };
  #     device = {
  #       name = "kensington-usb-orbit";
  #       middle_button_emulation = 1;
  #     };
  #     ecosystem.no_update_news = true;
  #     bind = [
  #       "$mainMod, Return, exec, footclient tmux"
  #       "$mainMod, A, exec, vol.sh"
  #       "$mainMod SHIFT, Q, killactive,"
  #       "$mainMod SHIFT CTRL, Z, exit,"
  #       "$mainMod, V, togglefloating"
  #       "$mainMod, C, exec, firejail firefox"
  #       "$mainMod SHIFT, C, exec, firejail firefox -P work"
  #       "$mainMod, U, exec, emacsclient -c -a emacs"
  #       "$mainMod, P, exec, bemenu-run"
  #       "$mainMod SHIFT, O, exec, mpc next"
  #       "$mainMod SHIFT, I, exec, mpc prev"
  #       "$mainMod SHIFT, P, exec, mpc toggle"
  #       "$mainMod SHIFT, Prior, exec, light -A 2"
  #       "$mainMod SHIFT, Next, exec, light -U 2"
  #       "$mainMod SHIFT, Home, exec, light.sh"
  #       "$mainMod, M, exec, Menu"
  #       "ALT, Tab, exec, show.sh"
  #       "$mainMod, Y, exec, clipshow.sh"
  #       "$mainMod, F, fullscreen, 0"
  #       "$mainMod SHIFT , F, fullscreenstate, 0 2"
  #       # "$mainMod, W, focusmonitor, HDMI-A-1"
  #       # "$mainMod, E, focusmonitor, eDP-1"
  #       # "$mainMod, R, focusmonitor, HDMI-A-2"
  #       "$mainMod, E, focusmonitor, eDP-1"
  #       "$mainMod, W, focusmonitor, HDMI-A-1"
  #       "$mainMod, h, movefocus, l"
  #       "$mainMod, l, movefocus, r"
  #       "$mainMod, j, movefocus, u"
  #       "$mainMod, k, movefocus, d"
  #       "$mainMod SHIFT, h, movewindow, l"
  #       "$mainMod SHIFT, l, movewindow, r"
  #       "$mainMod SHIFT, j, movewindow, u"
  #       "$mainMod SHIFT, k, movewindow, d"
  #       "$mainMod CTRL, l, resizeactive, 10 0"
  #       "$mainMod CTRL, h, resizeactive, -10 0"
  #       "$mainMod CTRL, j, resizeactive, 0 -10"
  #       "$mainMod CTRL, k, resizeactive, 0 10"
  #       "$mainMod, 1, focusworkspaceoncurrentmonitor, 1"
  #       "$mainMod, 2, focusworkspaceoncurrentmonitor, 2"
  #       "$mainMod, 3, focusworkspaceoncurrentmonitor, 3"
  #       "$mainMod, 4, focusworkspaceoncurrentmonitor, 4"
  #       "$mainMod, 5, focusworkspaceoncurrentmonitor, 5"
  #       "$mainMod, 6, focusworkspaceoncurrentmonitor, 6"
  #       "$mainMod, 7, focusworkspaceoncurrentmonitor, 7"
  #       "$mainMod, 8, focusworkspaceoncurrentmonitor, 8"
  #       "$mainMod, 9, focusworkspaceoncurrentmonitor, 9"
  #       "$mainMod SHIFT, 1, movetoworkspacesilent, 1"
  #       "$mainMod SHIFT, 2, movetoworkspacesilent, 2"
  #       "$mainMod SHIFT, 3, movetoworkspacesilent, 3"
  #       "$mainMod SHIFT, 4, movetoworkspacesilent, 4"
  #       "$mainMod SHIFT, 5, movetoworkspacesilent, 5"
  #       "$mainMod SHIFT, 6, movetoworkspacesilent, 6"
  #       "$mainMod SHIFT, 7, movetoworkspacesilent, 7"
  #       "$mainMod SHIFT, 8, movetoworkspacesilent, 8"
  #       "$mainMod SHIFT, 9, movetoworkspacesilent, 9"
  #       "$mainMod SHIFT, 0, movetoworkspacesilent, 10"
  #     ];
  #     bindm = [
  #       "$mainMod, mouse:272, movewindow"
  #       "$mainMod, mouse:273, resizewindow"
  #     ];
  #     "$mainMod" = "SUPER";
  #   };
  #   extraConfig = ''
  #     # monitor=HDMI-A-1,1280x1024,0x0, 1
  #     # monitor=eDP-1,1920x1080,1280x0, 1
  #     # monitor=HDMI-A-2,1280x1024,3200x0, 1
  #     monitor=eDP-1,1920x1080,1280x0, 1
  #     monitor=HDMI-A-1,1280x1024,0x0, 1

  #     exec-once = dunst
  #     exec-once = hyprpaper
  #     exec-once = hypridle
  #     exec-once = ~/.local/bin/batt.sh
  #     # exec-once = ~/.local/bin/dark.sh
  #     exec-once = wlsunset -S 07:00 -s 20:00 -T 4800 -t 2000
  #     exec-once = wl-paste --watch cliphist store
  #     exec-once = cliphist wipe
  #     # exec-once = hyprctl setcursor plan9 20
  #     # exec-once = dconf write /org/gnome/desktop/interface/cursor-theme "plan9"
  #     # exec-once = gsettings set org.gnome.desktop.interface cursor-theme 'plan9'

  #     # env = HYPRCURSOR_THEME,plan9
  #     # env = HYPRCURSOR_SIZE,20
  #     # env = XCURSOR_THEME,plan9
  #     # env = XCURSOR_SIZE,20

  #     windowrulev2 = workspace 1,class:^(firefox)$,
  #     windowrulev2 = workspace 2,class:^(emacs)$,
  #     windowrulev2 = workspace 8,fullscreen,class:^(mpv)$,
  #   '';
  # };
  services.hyprpaper = {
    enable = true;
    settings = {
      ipc = false;
      splash = false;
      preload = ["~/Downloads/Images/Dlowsat.png"];
      wallpaper = [",~/Downloads/Images/Dlowsat.png"];
    };
  };

  services.hypridle = {
    enable = true;
    settings = {
      general = {
        # after_sleep_cmd = "hyprctl dispatch dpms on";
        after_sleep_cmd = "niri msg action power-on-monitors";
        before_sleep_cmd = "hyprlock";
        ignore_dbus_inhibit = false;
        ignore_systemd_inhibit = false;
        lock_cmd = "hypridle";
      };
      listener = [
        {
          timeout = 900;
          on-timeout = "hypridle";
        }
        {
          timeout = 1200;
          # on-timeout = "hyprctl dispatch dpms off";
          # on-resume = "hyprctl dispatch dpms on";
          on-timeout = "niri msg action power-off-monitors";
          on-resume = "niri msg action power-on-monitors";
        }
      ];
    };
  };

  programs.hyprlock = {
    enable = true;
    settings = {
      general = [
        {
          monitor = "";
          ignore_empty_input = true;
          hide_cursor = true;
          no_fade_in = true;
          no_fade_out = true;
        }
      ];
      animations = [{enabled = false;}];
      background = [{color = "rgb(0, 0, 0)";}];
      input-field = [
        {
          size = "300, 50";
          position = "0, -80";
          halign = "center";
          valign = "center";
          monitor = "";
          dots_center = true;
          fade_on_empty = true;
          font_color = "rgb(202, 211, 245)";
          # inner_color = "rgb(91, 96, 120)";
          inner_color = "rgb(0, 0, 0)";
          # outer_color = "rgb(24, 25, 38)";
          outer_color = "rgb(0, 0, 0)";
          outline_thickness = 2;
          placeholder_text = "";
          shadow_passes = 0;
          rounding = 0;
        }
      ];
    };
  };
}
