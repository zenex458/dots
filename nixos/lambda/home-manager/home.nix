# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{ inputs
, outputs
, lib
, config
, pkgs
, ...
}: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/home-manager):
    # outputs.homeManagerModules.example

    # Or modules exported from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModules.default

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
  ];

  nixpkgs = {
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "zenex";
    homeDirectory = "/home/zenex";
  };
  manual.manpages.enable = true;
  programs.man.enable = true;
  programs.man.generateCaches = true;

  services.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
  };

  wayland.windowManager.hyprland = {
    enable = true;
    extraConfig = ''
      monitor=eDP-1, 1920x1080, 1390x0, 1
      monitor=HDMI-A-1, 1280x1024, 0x0, 1
      # See https://wiki.hyprland.org/Configuring/Keywords/ for more

      # Execute your favorite apps at launch
      exec-once = waybar
      exec-once = gammastep.sh
      exec-once = dark.sh
      exec-once = dunst
      exec-once = batt.sh
      exec-once = foot --server
      exec-once = hyprpaper


      # Source a file (multi-file configs)
      # source = ~/.config/hypr/myColors.conf

      # Set programs that you use
      $terminal = foot tmux
      $menu = bemenu-run
      # Some default env vars.
      env = XCURSOR_SIZE,24

      # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
      input {
          kb_layout = gb
          kb_variant =
          kb_model =
          kb_options = altwin:ctrl_alt_win
          kb_rules =
      	repeat_delay= 200
      	repeat_rate= 40

          follow_mouse = 1

          touchpad {
              natural_scroll = false
          }

          sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
      }

      general {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more
      	cursor_inactive_timeout = 5
          gaps_in = 0
          gaps_out = 0
          border_size = 2
          col.active_border = 0x88888888
          col.inactive_border = 0x000000

          layout = master

          # Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
          allow_tearing = false
      }

      misc {
      	disable_hyprland_logo = true
      	disable_splash_rendering = true
      	new_window_takes_over_fullscreen = true
      };
      decoration {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more

          rounding = 0

          blur {
              enabled = false
              size = 3
              passes = 1

              vibrancy = 0.1696
          }

          drop_shadow = false
          shadow_range = 4
          shadow_render_power = 3
          col.shadow = rgba(1a1a1aee)
      }

      animations {
          enabled = false
          # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more

          bezier = myBezier, 0.05, 0.9, 0.1, 1.05

          animation = windows, 1, 7, myBezier
          animation = windowsOut, 1, 7, default, popin 80%
          animation = border, 1, 10, default
          animation = borderangle, 1, 8, default
          animation = fade, 1, 7, default
          animation = workspaces, 1, 6, default
      }

      dwindle {
          # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
          pseudotile = true # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
          preserve_split = true # you probably want this
      }

      master {
          # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
          new_is_master = true
      	no_gaps_when_only = 1
      }

      gestures {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more
          workspace_swipe = false
      }

      misc {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more
          force_default_wallpaper = 0 # Set to 0 to disable the anime mascot wallpapers
          disable_autoreload = true
      }

      # Example per-device config
      # See https://wiki.hyprland.org/Configuring/Keywords/#per-device-input-configs for more
      device:epic-mouse-v1 {
          sensitivity = -0.5
      }

      # Example windowrule v1
      # windowrule = float, ^(kitty)$
      # Example windowrule v2
      # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
      # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
      windowrule=float,title:^(pulsemixer)(.*)$
      windowrulev2 = nomaximizerequest, class:.* # You'll probably like this.


      # See https://wiki.hyprland.org/Configuring/Keywords/ for more
      $mainMod = SUPER

      # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
      bind = $mainMod, RETURN, exec, $terminal
      bind = $mainMod SHIFT, Q, killactive,
      bind = $mainMod SHIFT CONTROL, L, exec, Menu
      bind = $mainMod, A, exec, foot -T pulsemixer pulsemixer
      bind = $mainMod SHIFT, SPACE, togglefloating,
      bind = $mainMod, P, exec, $menu
      bind = $mainMod, F, fullscreen,1
      bind = $mainMod, B, exec, pkill -SIGUSR1 waybar
      bind = $mainMod, C, exec, firefox
      bind = $mainMod SHIFT, C, exec, firefox --private-window
      bind = $mainMod SHIFT CONTROL, C, exec, firefox -P priv
      bind = $mainMod SHIFT, O, exec, mpc next
      bind = $mainMod SHIFT, I, exec, mpc prev
      bind = $mainMod SHIFT, P, exec, mpc toggle
      bind = $mainMod, U, exec, emacsclient -c -a emacs
      bind = $mainMod SHIFT, BRACKETLEFT, exec, amixer sset Master 2%-
      bind = $mainMod SHIFT, BRACKETRIGHT, exec, amixer sset Master 2%+
      bind = $mainMod SHIFT, PRIOR, exec, light -A 2
      bind = $mainMod SHIFT, NEXT, exec, light -U 2
      bind = $mainMod SHIFT CONTROL, Z, exec, swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'hyprctl dispatch exit'

      # Move focus with mainMod + arrow keys
      bind = $mainMod, H, movefocus, l
      bind = $mainMod, L, movefocus, r
      bind = $mainMod, K, movefocus, u
      bind = $mainMod, J, movefocus, d

      # Switch workspaces with mainMod + [0-9]
      bind = $mainMod, 1, workspace, 1
      bind = $mainMod, 2, workspace, 2
      bind = $mainMod, 3, workspace, 3
      bind = $mainMod, 4, workspace, 4
      bind = $mainMod, 5, workspace, 5
      bind = $mainMod, 6, workspace, 6
      bind = $mainMod, 7, workspace, 7
      bind = $mainMod, 8, workspace, 8
      bind = $mainMod, 9, workspace, 9
      bind = $mainMod, 0, workspace, 10

      # Move active window to a workspace with mainMod + SHIFT + [0-9]
      bind = $mainMod SHIFT, 1, movetoworkspacesilent, 1
      bind = $mainMod SHIFT, 2, movetoworkspacesilent, 2
      bind = $mainMod SHIFT, 3, movetoworkspacesilent, 3
      bind = $mainMod SHIFT, 4, movetoworkspacesilent, 4
      bind = $mainMod SHIFT, 5, movetoworkspacesilent, 5
      bind = $mainMod SHIFT, 6, movetoworkspacesilent, 6
      bind = $mainMod SHIFT, 7, movetoworkspacesilent, 7
      bind = $mainMod SHIFT, 8, movetoworkspacesilent, 8
      bind = $mainMod SHIFT, 9, movetoworkspacesilent, 9
      bind = $mainMod SHIFT, 0, movetoworkspacesilent, 10

      # Move/resize windows with mainMod + LMB/RMB and dragging
      bindm = $mainMod, mouse:272, movewindow
      bindm = $mainMod, mouse:273, resizewindow
    '';
  };
  programs.swaylock = {
    enable = true;
    settings = {
      color = "000000";
      indicator-idle-visible = false;
      indicator-radius = 100;
      show-failed-attempts = true;
    };
  };
  services.swayidle = {
    enable = true;
    events = [
      { event = "before-sleep"; command = "${pkgs.swaylock}/bin/swaylock"; }
    ];
    timeouts = [
      { timeout = 1800; command = "${pkgs.swaylock}/bin/swaylock -fF"; }
    ];
  };
  programs.foot = {
    enable = true;
    settings = {
      main = {
        term = "xterm-256color";
        font = "IosevkaTerm Nerd Font Mono:size=8";
        notify = "notify-send -a \${app-id} -i \${app-id} \${title} \${body}";
        dpi-aware = "yes";
      };
      cursor = {
        blink = "yes";
      };
      mouse = {
        hide-when-typing = "yes";
      };
      colors = {
        background = "000000";
        foreground = "c6c6c6";
        ## Normal/regular colors (color palette 0-7)
        regular0 = "000000"; #black
        regular1 = "cd0000"; #red
        regular2 = "00cd00"; #green
        regular3 = "cdcd00"; #yellow
        regular4 = "2B7Df0"; #blue
        regular5 = "cd00cd"; #magenta
        regular6 = "00cdcd"; #cyan
        regular7 = "e5e5e5"; #white

        ## Bright colors (color palette 8-15)
        bright0 = "808080"; #bright black
        bright1 = "ff0000"; #bright red
        bright2 = "00ff00"; #bright green
        bright3 = "ffff00"; #bright yellow
        bright4 = "0066FF"; #bright blue
        bright5 = "ff00ff"; #bright magenta
        bright6 = "00ffff"; #bright cyan
        bright7 = "ffffff"; #bright white
      };
    };
  };
  wayland.windowManager.sway.swaynag.enable = true;
  wayland.windowManager.sway = {
    enable = true;
    xwayland = true;
    config = {
      keybindings = {
        "Mod4+Return" = "exec ${pkgs.foot}/bin/footclient tmux";
        #        "Mod4+Return" = "exec footclient tmux";
        "Mod4+1" = "workspace number 1";
        "Mod4+2" = "workspace number 2";
        "Mod4+3" = "workspace number 3";
        "Mod4+4" = "workspace number 4";
        "Mod4+5" = "workspace number 5";
        "Mod4+6" = "workspace number 6";
        "Mod4+7" = "workspace number 7";
        "Mod4+8" = "workspace number 8";
        "Mod4+9" = "workspace number 9";
        "Mod4+Shift+1" = "move container to workspace number 1";
        "Mod4+Shift+2" = "move container to workspace number 2";
        "Mod4+Shift+3" = "move container to workspace number 3";
        "Mod4+Shift+4" = "move container to workspace number 4";
        "Mod4+Shift+5" = "move container to workspace number 5";
        "Mod4+Shift+6" = "move container to workspace number 6";
        "Mod4+Shift+7" = "move container to workspace number 7";
        "Mod4+Shift+8" = "move container to workspace number 8";
        "Mod4+Shift+9" = "move container to workspace number 9";
        "Mod4+greater" = "move workspace to output right";
        "Mod4+Shift+comma" = "move workspace to output left";
        "Mod4+Shift+z" = "reload";
        "Mod4+c" = "exec firefox";
        "Mod4+Shift+c" = "exec firefox --private-window";
        "Mod4+Shift+Control+c" = "exec firefox -P priv";
        "Mod4+Shift+o" = "exec mpc next";
        "Mod4+Shift+i" = "exec mpc prev";
        "Mod4+Shift+p" = "exec mpc toggle";
        "Mod4+u" = "exec emacsclient -c -a emacs";
        "Mod4+Shift+Control+l" = "Menu";
        "Mod4+a" = "exec ${pkgs.foot}/bin/footclient pulsemixer";
        "Mod4+Shift+bracketleft" = "exec amixer sset Master 2%-";
        "Mod4+Shift+bracketright" = "exec amixer sset Master 2%+";
        "Mod4+Shift+Prior" = "exec light -A 2";
        "Mod4+Shift+Next" = "exec light -U 2";
        "Mod4+Shift+e" = "exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'";
        "Mod4+Shift+h" = "move left";
        "Mod4+Shift+j" = "move down";
        "Mod4+Shift+k" = "move up";
        "Mod4+Shift+l" = "move right";
        "Mod4+Shift+minus" = "move scratchpad";
        "Mod4+Shift+q" = "kill";
        "Mod4+Shift+space" = "floating toggle";
        "Mod4+z" = "focus parent";
        "Mod4+b" = "splith";
        "Mod4+p" = "exec bemenu-run";
        "Mod4+e" = "layout toggle split";
        "Mod4+f" = "fullscreen toggle";
        "Mod4+h" = "focus left";
        "Mod4+j" = "focus down";
        "Mod4+k" = "focus up";
        "Mod4+l" = "focus right";
        "Mod4+minus" = "scratchpad show";
        "Mod4+r" = "mode resize";
        "Mod4+space" = "focus mode_toggle";
        "Mod4+v" = "splitv";
      };
      modes = {
        resize = {
          Escape = "mode default";
          Return = "mode default";
          h = "resize shrink width 10 px";
          j = "resize grow height 10 px";
          k = "resize shrink height 10 px";
          l = "resize grow width 10 px";
        };
      };
      colors = {
        focused = {
          border = "#000000";
          background = "#666666";
          text = "#000000";
          indicator = "#666666";
          childBorder = "#666666";
        };
        focusedInactive = {
          border = "#000000";
          background = "#222222";
          text = "#ffffff";
          indicator = "#000000";
          childBorder = "#000000";
        };
        unfocused = {
          border = "#000000";
          background = "#666666";
          text = "#ffffff";
          indicator = "#000000";
          childBorder = "#000000";
        };
        urgent = {
          border = "#FF0000";
          background = "#FF0000";
          text = "#000000";
          indicator = "#900000";
          childBorder = "#900000";
        };
      };
      fonts = {
        names = [ "Iosevka" ];
        style = "Bold";
        size = 10.0;
      };
      bars = [{
        fonts = {
          names = [ "Iosevka" ];
          style = "Regular";
          size = 10.0;
        };
        position = "top";
        statusCommand = "while ~/.config/sway/status.sh; do sleep 1; done";
        hiddenState = "hide";
        mode = "hide";
        extraConfig = "modifier mod4";
        colors = {
          background = "#000000";
          activeWorkspace = {
            border = "#000000";
            background = "#666666";
            text = "#000000";
          };
          inactiveWorkspace = {
            border = "#000000";
            background = "#222222";
            text = "#ffffff";
          };
          focusedWorkspace = {
            border = "#000000";
            background = "#666666";
            text = "#ffffff";
          };
          urgentWorkspace = {
            border = "#FF0000";
            background = "#FF0000";
            text = "#000000";
          };

        };
      }];
      modifier = "Mod4";
      input = {
        "type:keyboard" = {
          xkb_layout = "gb";
          xkb_options = "altwin:ctrl_alt_win";
          repeat_delay = "200";
          repeat_rate = "40";
        };
        "type:touchpad" = {
          tap = "enabled";
          natural_scroll = "disabled";
          dwt = "enabled";
        };
        "1149:4130:Kensington_USB_Orbit" = {
          natural_scroll = "disabled";
          drag = "enabled";
          drag_lock = "enabled";
          middle_emulation = "enabled";
          #        map_to_output = "\"LG Display 0x04A9 Unknown\"";
        };
        "type:pointer" = {
          dwt = "enabled";
        };
      };
      startup = [
        { command = "autotiling"; always = true; }
        { command = "gammastep.sh"; }
        { command = "dark.sh"; }
        { command = "dunst"; }
        { command = "batt.sh"; }
        { command = "foot --server"; }
      ];
      floating = {
        border = 8;
        titlebar = true;
        criteria = [
          {
            title = "pulsemixer";
          }
        ];
      };
      window = {
        titlebar = false;
        hideEdgeBorders = "both";
      };
      output = {
        "*" = {
          bg = "~/Downloads/Images/sriver.jpg fill";
        };
        "eDP-1" = {
          pos = "1390 0 res 1920x1080";
        };
        "HDMI-A-1" = {
          pos = "0 0 res 1280x1024";
        };

      };
    };
  };

  qt = {
    enable = true;
    platformTheme = "gnome";
    style.package = pkgs.adwaita-qt;
    style.name = "adwaita-dark";
  };

  gtk = {
    enable = true;
    theme.package = pkgs.shades-of-gray-theme;
    theme.name = "Shades-of-gray";
    iconTheme.package = pkgs.paper-icon-theme;
    iconTheme.name = "Paper-Mono-Dark";
    cursorTheme.name = "Paper";
    font.package = pkgs.iosevka;
    font.name = "Iosevka Extended";
    font.size = 10;
  };
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
  };
  programs.firefox = {
    enable = true;
    profiles.priv = {
      id = 1;
      search.engines = {
        "Bing".metaData.hidden = true;
        "Google".metaData.hidden = true;
        "Amazon.co.uk".metaData.hidden = true;
        "eBay".metaData.hidden = true;
      };
      search.force = true;
      userChrome =
        ''
          /* https://gist.github.com/chris-vecchio/d6a47fc733559752cc3a09937381d7ae */
          /* Firefox userChrome.css */

          /*** PROTON TABS TWEAKS ***/
          /* SOURCE: modified version of https://www.userchrome.org/firefox-89-styling-proton-ui.html#tabstyler */
          /* Make tab shape square */
          #tabbrowser-tabs {
            --user-tab-rounding: 0px;
          }

          .tab-background {
            border-radius: var(--user-tab-rounding) var(--user-tab-rounding) 0px 0px !important;
            margin-block: 1px 0 !important;
          }

          /* Borders on tab scroll right and left buttons */
          #scrollbutton-up, #scrollbutton-down { /* 6/10/2021 */
            border-top-width: 1px !important;
            border-bottom-width: 0 !important;
          }

          /* Inactive tabs: Separator line style */
          /* For light backgrounds */
          .tabbrowser-tab:not([selected=true]):not([multiselected=true]):not([beforeselected-visible="true"]) .tab-background {
            border-right: 1px solid var(--lwt-background-tab-separator-color, rgba(0, 0, 0, .20)) !important;
          }

          /* For dark backgrounds */
          [brighttext="true"] .tabbrowser-tab:not([selected=true]):not([multiselected=true]):not([beforeselected-visible="true"]) .tab-background {
            border-right: 1px solid var(--lwt-background-tab-separator-color, var(--lwt-selected-tab-background-color, rgba(255, 255, 255, .20))) !important;
          }

          .tabbrowser-tab:not([selected=true]):not([multiselected=true]) .tab-background {
            border-radius: 0 !important;
          }

          /* Remove padding between tabs */
          .tabbrowser-tab {
            padding-left: 0 !important;
            padding-right: 0 !important;
          }

          /* Set tab fill color and text color */
          #TabsToolbar {
            background-color: #202340;
            color: #F9F9FA;
          }
          /*** END PROTON TABS TWEAKS ***/


          /*** TIGHTEN UP DROP-DOWN/CONTEXT/POPUP MENU SPACING ***/
          /* SOURCE: https://www.userchrome.org/firefox-89-styling-proton-ui.html#menuspacing */
          menupopup > menuitem, menupopup > menu {
            padding-block: 4px !important;
          }

          /* Tighten up hamburger menu spacing and square the edges */
          :root {
            --arrowpanel-menuitem-padding: 2px !important;
            --arrowpanel-border-radius: 0px !important;
            --arrowpanel-menuitem-border-radius: 0px !important;
          }
          /*** END TIGHTEN UP DROP-DOWN/CONTEXT/POPUP MENU SPACING ***/

        '';
      settings = {
        "browser.translations.enable" = false;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.server" = "data: =";
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.updatePing.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.coverage.opt-out" = true;
        "toolkit.coverage.opt-out" = true;
        "toolkit.coverage.endpoint.base" = "";
        "browser.ping-centre.telemetry" = false;

      };

    };
    profiles."work" = {
      search.engines = {
        "Bing".metaData.hidden = true;
        "Google".metaData.hidden = true;
        "Amazon.co.uk".metaData.hidden = true;
        "eBay".metaData.hidden = true;
        "Startpage" = {
          urls = [{
            template = "https://www.startpage.com/do/search";
            params = [
              { name = "query"; value = "{searchTerms}"; }
            ];
          }];
        };

      };
      search.default = "Startpage";
      search.force = true;
      userChrome =
        ''
          /* https://gist.github.com/chris-vecchio/d6a47fc733559752cc3a09937381d7ae */
          /* Firefox userChrome.css */

          /*** PROTON TABS TWEAKS ***/
          /* SOURCE: modified version of https://www.userchrome.org/firefox-89-styling-proton-ui.html#tabstyler */
          /* Make tab shape square */
          #tabbrowser-tabs {
            --user-tab-rounding: 0px;
          }

          .tab-background {
            border-radius: var(--user-tab-rounding) var(--user-tab-rounding) 0px 0px !important;
            margin-block: 1px 0 !important;
          }

          /* Borders on tab scroll right and left buttons */
          #scrollbutton-up, #scrollbutton-down { /* 6/10/2021 */
            border-top-width: 1px !important;
            border-bottom-width: 0 !important;
          }

          /* Inactive tabs: Separator line style */
          /* For light backgrounds */
          .tabbrowser-tab:not([selected=true]):not([multiselected=true]):not([beforeselected-visible="true"]) .tab-background {
            border-right: 1px solid var(--lwt-background-tab-separator-color, rgba(0, 0, 0, .20)) !important;
          }

          /* For dark backgrounds */
          [brighttext="true"] .tabbrowser-tab:not([selected=true]):not([multiselected=true]):not([beforeselected-visible="true"]) .tab-background {
            border-right: 1px solid var(--lwt-background-tab-separator-color, var(--lwt-selected-tab-background-color, rgba(255, 255, 255, .20))) !important;
          }

          .tabbrowser-tab:not([selected=true]):not([multiselected=true]) .tab-background {
            border-radius: 0 !important;
          }

          /* Remove padding between tabs */
          .tabbrowser-tab {
            padding-left: 0 !important;
            padding-right: 0 !important;
          }

          /* Set tab fill color and text color */
          #TabsToolbar {
            background-color: #202340;
            color: #F9F9FA;
          }
          /*** END PROTON TABS TWEAKS ***/


          /*** TIGHTEN UP DROP-DOWN/CONTEXT/POPUP MENU SPACING ***/
          /* SOURCE: https://www.userchrome.org/firefox-89-styling-proton-ui.html#menuspacing */
          menupopup > menuitem, menupopup > menu {
            padding-block: 4px !important;
          }

          /* Tighten up hamburger menu spacing and square the edges */
          :root {
            --arrowpanel-menuitem-padding: 2px !important;
            --arrowpanel-border-radius: 0px !important;
            --arrowpanel-menuitem-border-radius: 0px !important;
          }
          /*** END TIGHTEN UP DROP-DOWN/CONTEXT/POPUP MENU SPACING ***/

        '';
      settings = {
        "browser.search.region" = "GB";
        "browser.search.isUS" = false;
        "distribution.searchplugins.defaultLocale" = "en-GB";
        "intl.accept_languages" = "en-GB, en";
        "browser.translations.enable" = false;
        "keyword.enabled" = true;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "browser.shell.shortcutFavicons" = true;
        "extensions.pocket.enabled" = false;
        "identity.fxaccounts.enabled" = false;
        "permissions.default.geo" = 2;
        "webgl.disabled" = false;
        "media.videocontrols.picture-in-picture.enabled" = false;
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
        "privacy.clearOnShutdown.cookies" = true;
        "places.history.enabled" = true;
        "signon.rememberSignons" = false;
        "browser.cache.memory.enable" = false;
        "browser.cache.memory.capacity" = 0;
        "permissions.memory_only" = true;
        "browser.urlbar.suggest.history" = true;
        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.openpage" = true;
        "browser.urlbar.suggest.topsites" = false;
        "browser.urlbar.suggest.engines" = false;
        "browser.tabs.firefox-view" = false;
        "browser.tabs.firefox-view-newIcon" = false;
        "browser.tabs.firefox-view-next" = false;
        "browser.compactmode.show" = false;
        "browser.uidensity" = 1;
        "extensions.formautofill.creditCards.available" = false;
        "extensions.formautofill.addresses.enabled" = false;
        "extensions.formautofill.available" = "off";
        "extensions.formautofill.heuristics.enabled" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "signon.autofillForms" = false;
        "privacy.resistFingerprinting.letterboxing" = false;
        "media.eme.enabled" = true;
        "general.smoothScroll" = false;
        "media.hardwaremediakeys.enabled" = false;
        "beacon.enabled" = false;
        "browser.link.open_newwindow" = 3;
        "browser.startup.page" = 0;
        "privacy.clearOnShutdown.formdata" = true;
        "browser.urlbar.trimURLs" = false;
        "extensions.getAddons.showPane" = false;
        "extensions.htmlaboutaddons.recommendations.enabled" = false;
        "browser.discovery.enabled" = false;
        "browser.shopping.experience2023.enabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        "datareporting.healthreport.uploadEnabled" = false;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.server" = "data: =";
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.updatePing.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.coverage.opt-out" = true;
        "toolkit.coverage.opt-out" = true;
        "toolkit.coverage.endpoint.base" = "";
        "browser.ping-centre.telemetry" = false;
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "app.shield.optoutstudies.enabled" = false;
        "app.normandy.enabled" = false;
        "app.normandy.api_url" = "";
        "breakpad.reportURL" = "";
        "browser.tabs.crashReporting.sendReport" = false;
        "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;
        "captivedetect.canonicalURL" = "";
        "network.captive-portal-service.enabled" = false;
        "network.connectivity-service.enabled" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
        "browser.download.useDownloadDir" = false;
        "browser.download.always_ask_before_handling_new_types" = true;
        "browser.search.suggest.enabled" = false;
        "browser.urlbar.suggest.searches" = false;
        "browser.newtabpage.enabled" = false;
        "browser.newtabpage.activity-stream.showSponsored" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.newtabpage.activity-stream.default.sites" = "";
        "dom.security.https_only_mode" = true;
        "dom.security.https_only_mode_ever_enabled" = true;
        "privacy.clearOnShutdown.offlineApps" = true;
        "privacy.sanitize.sanitizeOnShutdown" = true;
        "privacy.clearOnShutdown.cache" = true;
        "privacy.clearOnShutdown.downloads" = true;
        "privacy.clearOnShutdown.sessions" = true;
        "privacy.clearOnShutdown.history" = false;
        "privacy.cpd.history" = true;
        "browser.formfill.enable" = false;
        "privacy.clearOnShutdown.siteSettings" = true;
        "privacy.cpd.siteSettings" = true;
        "signon.management.page.breach-alerts.enabled" = false;
        "intl.regional_prefs.use_os_locales" = true;
        "extensions.screenshots.disabled" = true;
      };
    };
  };
  home.sessionPath = [ "$HOME/.local/bin" ];
  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };
  programs.bash = {
    enableCompletion = true;
    historyControl = [ "ignoredups" ];
    historyFile = "$HOME/.local/share/.bash_history";
    historyFileSize = 10000;
    historySize = 10000;
    enable = true;
    shellOptions = [ "cdspell" "autocd" "histappend" ];
    bashrcExtra = ''
      bind 'set show-all-if-ambiguous on'
      bind 'set completion-ignore-case on'
      bind 'TAB:menu-complete'
    '';
    shellAliases = {
      upd = "sudo nixos-rebuild switch --flake ~/lambda/#eukaryotic";
      updflake = "nix flake update --commit-lock-file";
      #      upd = "sudo nix-channel --update && sudo nixos-rebuild switch";
      #     enc = "sudo $EDITOR /etc/nixos/configuration.nix";
      #    updc = "sudo nixos-rebuild switch";
      nixgc = "nix-collect-garbage";
      remoldgen = "sudo nix-collect-garbage -d && upd";
      re = "systemctl reboot";
      off = "systemctl poweroff";
      nv = "nvim";
      ls = "ls -F -h --color=always --group-directories-first";
      ga = "git add";
      gc = "git commit -m";
      gp = "git push -u origin main";
      updoff = "upd && sleep 2 && off";
      updr = "upd && sleep 2 && re";
      grep = "grep -i --colour=always";
      mkdir = "mkdir -pv";
      mv = "mv -iv";
      cp = "cp -iv";
      rm = "rm -iv";
      ll = "ls -lA";
      tm = "ps auxww | grep";
      lines = "ls | wc -l";
      tk = "tmux kill-session";
      sss = "scrot -d 5 ~/Downloads/Images/ss/%Y-%m-%d_$wx$h.png";
      cco = "gcc -Wall";
      ytmp3 = "yt-dlp --progress -q -x -o '%(title)s.%(ext)s' --audio-format mp3 --audio-quality 0 --embed-thumbnail";
      ytflac_thum_chap = "yt-dlp --progress -q -x -o '%(title)s.%(ext)s' --audio-format flac --audio-quality 0 --embed-thumbnail --embed-chapters";
      ytflac_aud = "yt-dlp --progress -q -x -o '%(title)s.%(ext)s' --audio-format flac --audio-quality 0";
      yt10 = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=1080][fps=30]+bestaudio/best[height<=1080]'";
      yt7 = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=720][fps=30]+bestaudio/best[height<=720]'";
      yt7s = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --sponsorblock-remove sponsor --remux-video mp4 --embed-subs; --embed-chapters --write-auto-subs --sub-langs en -f 'bestvideo[height<=720][fps=30]+bestaudio/best[height<=720]'";
      ytb = "yt-dlp --progress -q -o '%(title)s.%(ext)s' --remux-video mp4 --embed-subs --embed-chapters --write-auto-subs --sub-langs en";
      xdup = "xrandr --output HDMI-1 --same-as eDP-1";
      xright = "xrandr --output eDP1 --auto --primary --output HDMI1 --auto --right-of eDP1";
      xup = "xrandr --output HDMI-1 --brightness 1.0";
      chnum = "stat -c '%a %n'";
      tas = "tmux attach-session";
      tls = "tmux list-session";
      tat = "tmux attach -t";
      msd = "sudo mount -m -v -o rw,noexec,uid=1000,gid=1000 UUID=04C3-E2B3 /run/media/zenex/musicsd";
      umsd = "sudo umount -v /run/media/zenex/musicsd";
      mhd = "sudo mount -v -t ntfs -m -o rw,noexec,uid=1000,gid=1000 UUID=742455142454DAA6 /run/media/zenex/seagate";
      umhd = "sudo umount -v /run/media/zenex/seagate && lsblk";
      sysdlist = "systemctl list-unit-files --type=service --state=enabled";
      rsy = "rsync -ahPz --info=progress2";
      del = "trash-put";
      fnx = "find . -type f -exec chmod 644 {} +";
      dnx = "find . -type d -exec chmod 755 {} +";
      shx = "find . -name '*.sh' -execdir chmod +x {} +";
      ctg = "gsettings set org.cinnamon.desktop.default-applications.terminal exec gnome-terminal";
      dow = "aria2c -c -s 16 -x 16 -k 1M -j 1";
      fmir = "netselect-apt";
      kremap = "setxkbmap -option altwin:ctrl_alt_win";
      krremap = "setxkbmap -option";
      chkfstab = "sudo findmnt --verify";
      emdr = "systemctl --user restart emacs";
      ssway = "exec sway";

    };
    sessionVariables = {
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";
      XDG_CACHE_HOME = "$HOME/.cache";
      MUPDFHISTFILE = "/tmp/.mupdf.history";
      RXVT_SOCKET = "$XDG_RUNTIME_DIR/urxvtd";
      URXVT_PERL_LIB = "~/.config/urxvt/ext";
      XSECURELOCK_SHOW_HOSTNAME = 0;
      XSECURELOCK_SHOW_USERNAME = 0;
      XSECURELOCK_FONT = "Iosevka";
      XSECURELOCK_WAIT_TIME_MS = 200000;
      WGETRC = "$XDG_CONFIG_HOME/wgetrc";
      DOTNET_CLI_TELEMETRY_OPTOUT = 1;
      TERMINAL = "foot";
      EDITOR = "emacsclient -c -a emacs";
      VISUAL = "emacsclient -c -a emacs";
      FZF_DEFAULT_OPTS = "-e --no-scrollbar --border=none --reverse --no-info";
      QT_QPA_PLATFORMTHEME = "qt5ct";
      LESSHISTFILE = "/tmp/.lesshst";
      MOZ_ENABLE_WAYLAND = "1";
      XDG_CURRENT_DESKTOP = "hyprland";
      XDG_SESSION_TYPE = "wayland";
      XDG_SESSION_DESKTOP = "hyprland";
    };
    initExtra = "PROMPT_COMMAND=\"\${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r\"";
  };
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };
  #  services.unclutter = {
  #    enable = true;
  #    timeout = 5;
  #    extraOptions = [ "exclude-root" "ignore-scrolling" "ignore-buttons" "start-hidden" ];
  #  };
  home.packages = with pkgs; [
    neovim
    nnn
    emacs29-pgtk
    git
    htop
    ffmpeg_6-full
    xterm
    alacritty
    tmux
    nnn
    trash-cli
    dmenu
    libreoffice
    hunspell
    hunspellDicts.en_GB-large
    hunspellDicts.en-gb-large
    dunst
    libnotify
    mupdf
    zathura
    bc
    p7zip
    zip
    unzip
    fuse3
    mpv
    mpvScripts.mpris
    keepassxc
    openssl
    alsa-utils
    pulsemixer
    shellcheck
    yt-dlp
    kdeconnect
    gcc
    astyle
    nixpkgs-fmt
    ormolu
    shfmt
    mpd
    mpc-cli
    ncmpcpp
    pandoc
    lxqt.lxqt-policykit
    texlive.combined.scheme-full
    xfce.thunar
    fd
    ripgrep
    opensnitch-ui
    virt-manager
    xdg-utils
    wayland
    autotiling
    grim
    slurp
    wl-clipboard
    hyprpaper
    bemenu
    wdisplays
    gammastep
    smartmontools
    dotnet-sdk_8
    ddcutil
    poppler_utils
    linuxKernel.packages.linux_6_5_hardened.ddcci-driver
    nodePackages.prettier
    waybar
    man-pages
    man-pages-posix
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science uk ]))
  ];

  # Enable home-manager and git
  programs.home-manager.enable = true;
  programs.git.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
