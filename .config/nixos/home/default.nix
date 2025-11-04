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
        ./programs/hyprland.nix
        ./programs/niri.nix
        ./programs/shell.nix
        ./programs/terminal.nix
        ./services.nix
        ./theming.nix
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
