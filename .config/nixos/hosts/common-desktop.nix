{
  inputs,
  pkgs,
  lib,
  config,
  ...
}:
{
  imports = [ inputs.niri.nixosModules.niri ];

  fonts = {
    packages = with pkgs; [
      iosevka
      vista-fonts
      noto-fonts
      noto-fonts-cjk-sans
      aileron
    ];
    fontconfig = {
      defaultFonts = {
        monospace = [ "Iosevka" ];
        sansSerif = [ "Iosevka" ];
        serif = [ "Iosevka" ];
      };
    };
  };

  environment = {
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
      FREETYPE_PROPERTIES = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
    };
    etc = {
      "firejail/firefox.local".text = ''
        # Enable native notifications.
        dbus-user.talk org.freedesktop.Notifications
        # Allow inhibiting screensavers.
        dbus-user.talk org.freedesktop.ScreenSaver
        # Allow screensharing under Wayland.
        dbus-user.talk org.freedesktop.portal.Desktop
        disable-mnt
        #private-bin dbus-launch,dbus-send,firefox,which
        #private-bin dbus-launch,dbus-send,firefox,which
      '';
      "firejail/firefox-common.local".text = ''
        # private-etc fonts,group,hosts,localtime,nsswitch.conf,pki,pulse,resolv.conf,ssl
        #private-etc fonts,group,hosts,localtime,pulse
        private-tmp
      '';
      # "firejail/nolocal.net" = {
      #   source = "${pkgs.firejail}/etc/firejail/nolocal.net";
      #   mode = "0644";
      # };
    };
    defaultPackages = lib.mkForce [ ];
    systemPackages = with pkgs; [
      git
      vim
      emacs-nox
      tmux
      sbctl
    ];
    pathsToLink = [
      "/share/bash-completion"
      "/share/xdg-desktop-portal"
      "/share/applications"
      "/share/zsh"
    ];
  };

  services = {
    upower.enable = true;
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      wireplumber.enable = true;
    };

    tlp = {
      enable = true;
      settings = {
        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";
        PLATFORM_PROFILE_ON_AC = "performance";
        PLATFORM_PROFILE_ON_BAT = "low-power";
        START_CHARGE_THRESH_BAT0 = 75;
        STOP_CHARGE_THRESH_BAT0 = 80;
      };
    };

    locate = {
      enable = true;
      package = pkgs.plocate;
      output = /var/cache/locate/locatedb;
      interval = "hourly";
      pruneNames = [
        ".bzr"
        ".cache"
        ".git"
        ".hg"
        ".svn"
        ".ccls-cache"
        "*env*"
      ];
    };
  };

  niri-flake.cache.enable = false;
  programs = {
    fuse.userAllowOther = true;
    gamescope.enable = true;
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
    };
    ghidra = {
      enable = true;
      package = pkgs.unstable.ghidra;
    };
    wireshark.enable = true;
    command-not-found.enable = false;
    nix-index = {
      enable = true;
      enableBashIntegration = true;
      # enableFishIntegration = true;
      enableZshIntegration = true;
    };
    nix-ld = {
      enable = true;
      libraries = [
        pkgs.libpthread-stubs
        pkgs.ell
        pkgs.xorg.libxshmfence
        pkgs.xorg.libXScrnSaver
        pkgs.xprintidle-ng
        pkgs.libGL
        pkgs.pipewire
      ];
    };
    obs-studio.enable = true;
    virt-manager.enable = true;
    niri = {
      enable = true;
      package = pkgs.niri;
    };
    # adb.enable = true;
    localsend.enable = true;
    # ssh.startAgent = true;
    gnupg.agent.enable = true;
    firejail = {
      enable = false;
      wrappedBinaries = {
        firefox = {
          executable = "${pkgs.firefox}/bin/firefox";
          profile = "${pkgs.firejail}/etc/firejail/firefox.profile";
          extraArgs = [
            # Enforce dark mode
            "--env=GTK_THEME=Adwaita:dark"
          ];
        };
      };
    };
    light.enable = true;
    dconf.enable = true;
    # fish.enable = true;
    zsh.enable = true;
    bash.promptInit = ''
      if [ "$LOGNAME" = root ] || [ "$(id -u)" -eq 0 ]; then
      	PS1="\[\e[01;31m\]\[\u@\h:\w\n# \]\\[\e[00m\]"
      else
      	PS1="[\w]\n$ "
      fi
    '';
  };
}
