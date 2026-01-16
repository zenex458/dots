{
  inputs,
  pkgs,
  ...
}: {
  home-manager = {
    extraSpecialArgs = {inherit inputs;};
    users.zenex = {
      imports = [
        inputs.impermanence.nixosModules.home-manager.impermanence
        ../overlays
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
      nixpkgs = {
        # Configure your nixpkgs instance
        config = {
          allowUnfree = true;
          # Workaround for https://github.com/nix-community/home-manager/issues/2942
          allowUnfreePredicate = _: true;
        };
      };
      dconf.settings = {
        "org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
        };
        "org/virt-manager/virt-manager/connections" = {
          autoconnect = ["qemu:///system"];
          uris = ["qemu:///system"];
        };
      };
      gtk = {
        enable = true;
        theme = {
          name = "Adwaita";
        };
        gtk3 = {
          extraConfig = {
            gtk-application-prefer-dark-theme = 1;
          };
        };
        gtk4 = {
          extraConfig = {
            gtk-application-prefer-dark-theme = 1;
          };
        };
        font = {
          name = "Iosevka";
          size = 10;
        };
      };
      qt = {
        enable = true;
        style.name = "bb10dark";
      };
      programs = {
        mpv = {
          enable = true;
          config = {screenshot-directory = "~/Downloads/Images/ss/mpv";};
        };
        keepassxc = {
          enable = true;
          settings = {
            Browser = {
              Enabled = true;
              UpdateBinaryPath = false;
            };
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
          package = pkgs.ungoogled-chromium.override {enableWideVine = true;};
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
        home-manager.enable = true;
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
          TERMINAL = "kitty";
          EDITOR = "emacsclient -c -a emacs";
          VISUAL = "emacsclient -c -a emacs";
          LESSHISTFILE = "/tmp/.lesshst";
          MOZ_ENABLE_WAYLAND = 1;
          QT_QPA_PLATFORM = "wayland;xcb";
          GDK_BACKEND = "wayland";
          _JAVA_AWT_WM_NONREPARENTING = 1;
          SAL_USE_VCLPLUGIN = "gtk3";
          XCURSOR_SIZE = 20;
          BEMENU_OPTS = ''-i --fn 'Iosevka' -B '1' -f -p '>' -n --tb '#bdae93' --tf '#060606' --fb '#060606' --ff '#bdae93' --nb '#060606' --nf '#bdae93' --ab '#060606' --af '#bdae93' --sb '#060606' --sf '#bdae93' --cb '#bdae93' --cf '#bdae93' --hb '#bdae93' --hf '#060606' --sb '#bdae93' --sf '#060606' --scb '#060606' --scf '#bdae93' --bdr '#bdae93' '';
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
          ".config/vesktop/themes/theme.css" = {
            source = ../../vesktop/themes/theme.css;
          };
        };
        persistence."/persistent/home/zenex" = {
          directories = [
            ".config/emacs"
            ".config/feather"
            ".config/gh"
            ".config/netbird"
            ".config/opensnitch"
            ".config/Signal"
            ".config/simplex"
            ".config/vesktop"
            ".config/zotero"
            ".config/zsh"
            ".cargo"
            ".icons"
            ".local/share/fish"
            ".local/share/simplex"
            ".local/state/wireplumber"
            ".mozilla"
            "Dev"
            "Documents"
            "Downloads"
            "Music"
          ];
          files = [".local/share/.bash_history" ".cache/nix-index/files"];
          allowOther = true;
        };
        sessionPath = [
          "$HOME/.local/bin"
        ];
      };
      # Nicely reload system units when changing configs
      systemd.user.startServices = "sd-switch";
    };
  };
}
