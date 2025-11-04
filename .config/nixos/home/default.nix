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
        ./pkgs.nix
        ./programs/firefox.nix
        ./programs/hyprland.nix
        ./programs/niri.nix
        ./programs/emacs.nix
        ./programs/terminal.nix
        ./programs/shell.nix
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
        "org/virt-manager/virt-manager/connections" = {
          autoconnect = ["qemu:///system"];
          uris = ["qemu:///system"];
        };
      };

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

      xdg = {
        mime = {
          enable = true;
        };
        mimeApps = {
          enable = true;
          associations.added = {
            "text/markdown" = "emacs.desktop";
            "image/png" = "imv.desktop";
          };
          defaultApplications = {
            "text/plain" = "emacs.desktop";
            "text/html" = "firefox.desktop";
            "image/png" = "imv.desktop";
            "image/jpeg" = "imv.desktop";
            "image/gif" = "imv.desktop";
            "video/mp4" = "mpv.desktop";
            "audio/x-mpegurl" = "mpv.desktop";
            "application/pdf" = "org.pwmt.zathura.desktop";
            "application/vnd.ms-powerpoint" = "libreoffice-impress.desktop";
            "application/vnd.ms-powerpoint.presentation" = "libreoffice-impress.desktop";
            "application/vnd.ms-powerpoint.template" = "libreoffice-impress.desktop";
            "application/vnd.ms-word" = "libreoffice-writer.desktop";
            "application/vnd.ms-word.document" = "libreoffice-writer.desktop";
            "application/vnd.ms-word.template" = "libreoffice-writer.desktop";
            "x-scheme-handler/http" = "firefox.desktop";
            "x-scheme-handler/https" = "firefox.desktop";
            "x-scheme-handler/about" = "firefox.desktop";
            "x-scheme-handler/unknown" = "firefox.desktop";
            "inode/directory" = "nemo.desktop";
          };
        };
      };

      programs = {
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
        };
        persistence."/persistent/home/zenex" = {
          directories = [
            ".config/emacs"
            ".config/feather"
            ".config/gh"
            ".config/opensnitch"
            ".config/Signal"
            ".config/simplex"
            ".config/vesktop"
            ".config/zotero"
            ".config/zsh"
            ".config/netbird"
            ".local/share/simplex"
            ".local/state/wireplumber"
            ".mozilla"
            ".icons"
            "Dev"
            "Documents"
            "Downloads"
            "Music"
          ];
          files = [".local/share/.bash_history"];
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
