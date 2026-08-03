{
  inputs,
  pkgs,
  config,
  ...
}:
{
  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = ".backup";
    users.zenex = {
      imports = [
        ./mimeapps.nix
        ./pkgs.nix
        ./programs/emacs.nix
        ./programs/firefox.nix
        ./programs/sway.nix
        ./programs/niri.nix
        ./programs/shell.nix
        ./programs/terminal.nix
        ./services.nix
      ];
      systemd.user.services.polkit-gnome-authentication-agent-1 = {
        Unit = {
          Description = "polkit-gnome-authentication-agent-1";
          Wants = [ "graphical-session.target" ];
          After = [ "graphical-session.target" ];
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
        Service = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
      dconf.settings = {
        "org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
        };
        "org/virt-manager/virt-manager/connections" = {
          autoconnect = [ "qemu:///system" ];
          uris = [ "qemu:///system" ];
        };
        "org.gnome.desktop.wm.preferences" = {
          button-layout = "";
        };

      };
      gtk = {
        enable = true;
        theme.name = "Adwaita";
        gtk3 = {
          extraConfig = {
            gtk-application-prefer-dark-theme = 1;
            gtk-dialogs-use-header = false;
          };
          extraCss = ''
            /* No (default) title bar on wayland */
            headerbar.default-decoration {
              /* You may need to tweak these values depending on your GTK theme */
              margin-bottom: 50px;
              margin-top: -100px;
            }

            /* rm -rf window shadows */
            window.csd,             /* gtk4? */
            window.csd decoration {
              /* gtk3 */
              box-shadow: none;
            }
          '';

        };
        gtk4 = {
          theme = null;
          extraConfig.gtk-dialogs-use-header = false;
          extraCss = ''
            /* No (default) title bar on wayland */
            headerbar.default-decoration {
              /* You may need to tweak these values depending on your GTK theme */
              margin-bottom: 50px;
              margin-top: -100px;
            }

            /* rm -rf window shadows */
            window.csd,             /* gtk4? */
            window.csd decoration {
              /* gtk3 */
              box-shadow: none;
            }
          '';
        };
        font = {
          name = "Iosevka";
          size = 10;
        };
      };
      qt = {
        enable = true;
        platformTheme.name = "gtk";
        style.name = "kvantum";
      };

      programs = {
        home-manager.enable = true;
        qutebrowser = {
          enable = true;
          package = pkgs.qutebrowser.override {
            enableWideVine = true;
            withPdfReader = false;
            enableVulkan = true;
          };
          settings = {
            content = {
              private_browsing = true;
              blocking = {
                method = "both";
                adblock.lists = [
                  "https://easylist.to/easylist/easylist.txt"
                  "https://easylist.to/easylist/easyprivacy.txt"
                  "https://secure.fanboy.co.nz/fanboy-annoyance.txt"
                  "https://raw.githubusercontent.com/AdguardTeam/AdguardFilters/refs/heads/master/BaseFilter/sections/adservers.txt"
                  "https://raw.githubusercontent.com/AdguardTeam/AdguardFilters/refs/heads/master/BaseFilter/sections/adservers_firstparty.txt"
                  "https://raw.githubusercontent.com/uBlockOrigin/uAssets/refs/heads/master/filters/filters.txt"
                  "https://raw.githubusercontent.com/uBlockOrigin/uAssets/refs/heads/master/filters/ubol-filters.txt"
                  "https://raw.githubusercontent.com/uBlockOrigin/uAssets/refs/heads/master/filters/annoyances-others.txt"
                  "https://raw.githubusercontent.com/uBlockOrigin/uAssets/refs/heads/master/filters/privacy.txt"
                  "https://raw.githubusercontent.com/brave/adblock-lists/refs/heads/master/brave-lists/brave-firstparty.txt"
                  "https://adguardteam.github.io/HostlistsRegistry/assets/filter_3.txt"
                  "https://adguardteam.github.io/HostlistsRegistry/assets/filter_4.txt"
                  "https://adguardteam.github.io/HostlistsRegistry/assets/filter_7.txt"
                ];
              };
            };
            colors = {
              statusbar = {
                command.private = {
                  bg = "#060606";
                  fg = "#bdae93";
                };
                private = {
                  bg = "#060606";
                  fg = "#bdae93";
                };
              };
              completion = {
                item.selected.bg = "#c0c000";
                category = {
                  bg = "#2e2e2e";
                  fg = "#bdae93";
                };
                fg = [
                  "#bdae93"
                  "#bdae93"
                  "#bdae93"
                ];
                odd.bg = "#060606";
                even.bg = "#060606";
              };
              webpage.darkmode.enabled = true;
            };
            url = {
              default_page = "about:blank";
              start_pages = "about:blank";
            };
            tabs = {
              position = "left";
              show = "never";
            };
            scrolling.bar = "when-searching";
          };
          keyBindings = {
            normal = {
              ",m" = "spawn mpv {url}";
              ",M" = "hint links spawn mpv {hint-url}";
            };
          };
        };
        mpv = {
          enable = true;
          config.screenshot-directory = "~/Downloads/Images/ss/mpv";
        };
        keepassxc = {
          enable = true;
          settings = {
            Browser.Enabled = true;
            GUI = {
              AdvancedSettings = true;
              ApplicationTheme = "dark";
              CompactMode = true;
              HidePasswords = true;
            };
          };
        };
        chromium = {
          enable = true;
          package = pkgs.ungoogled-chromium.override { enableWideVine = true; };
        };
        zathura = {
          enable = true;
          mappings = {
            "<PageUp>" = "navigate previous";
            "<PageDown>" = "navigate next";
            "+" = "zoom in";
            "-" = "zoom out";
            "<C-q>" = "quit";
          };
        };
      };

      home = {
        stateVersion = "24.05";
        username = "zenex";
        sessionVariables = {
          XDG_CONFIG_HOME = "$HOME/.config";
          XDG_DATA_HOME = "$HOME/.local/share";
          XDG_STATE_HOME = "$HOME/.local/state";
          XDG_CACHE_HOME = "$HOME/.cache";
          MUPDFHISTFILE = "/tmp/.mupdf.history";
          DOTNET_CLI_TELEMETRY_OPTOUT = 1;
          TERMINAL = "foot";
          EDITOR = "emacsclient -c -a emacs";
          VISUAL = "emacsclient -c -a emacs";
          LESSHISTFILE = "/tmp/.lesshst";
          MOZ_ENABLE_WAYLAND = 1;
          QT_QPA_PLATFORM = "wayland;xcb";
          GDK_BACKEND = "wayland";
          _JAVA_AWT_WM_NONREPARENTING = 1;
          SAL_USE_VCLPLUGIN = "gtk3";
          XCURSOR_SIZE = 20;
          BEMENU_OPTS = "-i --fn 'Iosevka Bold' -B '1' -f -p '>' -n --tb '#bdae93' --tf '#060606' --fb '#060606' --ff '#bdae93' --nb '#060606' --nf '#bdae93' --ab '#060606' --af '#bdae93' --sb '#060606' --sf '#bdae93' --cb '#bdae93' --cf '#bdae93' --hb '#bdae93' --hf '#060606' --sb '#bdae93' --sf '#060606' --scb '#060606' --scf '#bdae93' --bdr '#bdae93' ";
          # MATHPATH = "/run/current-system/sw/share/man";
        };
        file = {
          ".local/bin" = {
            source = ../../../.local/bin;
            recursive = true;
            executable = true;
          };
          ".config/emacs" = {
            source = ../../emacs;
            recursive = true;
          };
          ".config/Vencord/themes/theme.css" = {
            source = ../../Vencord/themes/theme.css;
          };
          ".config/vesktop/themes/theme.css" = {
            source = ../../Vencord/themes/theme.css;
          };
        };
        persistence."/persistent" = {
          directories = [
            ".config/emacs"
            ".config/feather"
            ".config/gh"
            ".config/ghidra"
            ".config/gurk"
            ".config/nchat"
            ".config/opensnitch"
            ".config/Signal"
            ".config/Vencord"
            ".config/vesktop"
            ".config/discord"
            ".config/zotero"
            ".config/zsh"
            ".config/qutebrowser"
            ".icons"
            ".local/share/Steam"
            ".local/share/fish"
            ".local/share/qutebrowser"
            ".local/state/wireplumber"
            ".config/mozilla"
            ".steam"
            ".factorio"
            "Dev"
            "Documents"
            "Downloads"
            "Music"
          ];
          files = [
            ".local/share/.bash_history"
            ".cache/nix-index/files"
          ];
        };
        sessionPath = [ "$HOME/.local/bin" ];
      };
      # Nicely reload system units when changing configs
      systemd.user.startServices = "sd-switch";
    };
  };
}
