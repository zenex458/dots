{
  inputs,
  pkgs,
  lib,
  ...
}: {
  imports = [
    inputs.home-manager.nixosModules.home-manager
    inputs.impermanence.nixosModules.impermanence
    inputs.disko.nixosModules.disko
    inputs.lanzaboote.nixosModules.lanzaboote
    inputs.niri.nixosModules.niri
    ./hardened.nix
    ./disko-config.nix
    ./hardware-configuration.nix
    ./pkgs.nix
  ];
  boot = {
    # kernel.sysctl."vm.swappiness" = 30;
    supportedFilesystems = ["ntfs"];
    #kernelPackages = pkgs.linuxPackages_latest;
    #loader.systemd-boot.enable = true;
    loader.systemd-boot.enable = lib.mkForce false;
    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };
    loader.efi.canTouchEfiVariables = true;
    tmp.cleanOnBoot = true;

    initrd.postDeviceCommands = lib.mkAfter ''
      mkdir /btrfs_tmp
      mount /dev/pool/root /btrfs_tmp
      if [[ -e /btrfs_tmp/root ]]; then
          mkdir -p /btrfs_tmp/old_roots
          timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
          mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
      fi

      delete_subvolume_recursively() {
          IFS=$'\n'
          for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
              delete_subvolume_recursively "/btrfs_tmp/$i"
          done
          btrfs subvolume delete "$1"
      }

      for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30); do
          delete_subvolume_recursively "$i"
      done

      btrfs subvolume create /btrfs_tmp/root
      umount /btrfs_tmp
    '';
  };

  systemd.tmpfiles.rules = [
    "d /persistent/var/keys/ 0700 root root"
    "d /persistent/etc/nixos/ 0700 root root"
  ];

  programs.fuse.userAllowOther = true;
  home-manager = {
    extraSpecialArgs = {
      inherit inputs;
    };
    users.zenex = import ./home/zenex.nix;
  };

  networking = {
    timeServers = [
      "time.cloudflare.com"
      "ntppool1.time.nl"
      "nts.netnod.se"
      "ptbtime1.ptb.de"
    ];
    hostName = "nidus";
    wireless.iwd = {
      enable = true;
      settings = {
        General = {
          "EnableNetworkConfiguration" = true;
          "AddressRandomization" = "once";
          "AddressRandomizationRange" = "nic";
        };
        Settings = {
          AutoConnect = true;
        };
        Network = {
          NameResolvingService = "resolvconf";
        };
      };
    };
    enableIPv6 = false;
  };

  hardware = {
    graphics.enable = true;
    bluetooth = {
      enable = false;
      powerOnBoot = false;
    };
  };
  time.timeZone = "Europe/London";

  i18n = {
    defaultLocale = "en_GB.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "en_GB.UTF-8";
      LC_IDENTIFICATION = "en_GB.UTF-8";
      LC_MEASUREMENT = "en_GB.UTF-8";
      LC_MONETARY = "en_GB.UTF-8";
      LC_NAME = "en_GB.UTF-8";
      LC_NUMERIC = "en_GB.UTF-8";
      LC_PAPER = "en_GB.UTF-8";
      LC_TELEPHONE = "en_GB.UTF-8";
      LC_TIME = "en_GB.UTF-8";
    };
  };

  virtualisation = {
    libvirtd.enable = true;
    docker = {
      enable = true;
      storageDriver = "btrfs";
      rootless.setSocketVariable = true;
    };
  };
  niri-flake.cache.enable = true;
  programs = {
    obs-studio.enable = true;
    niri = {
      enable = true;
      package = pkgs.niri-stable;
    };
    # adb.enable = true;
    localsend.enable = true;
    ssh.startAgent = true;
    gnupg.agent.enable = true;
    xwayland.enable = true;
    # hyprland = {
    #   enable = true;
    #   withUWSM = true;
    #   xwayland.enable = true;
    # };
    firejail = {
      enable = true;
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
    zsh.enable = true;
    dconf.enable = true;
    bash.promptInit = ''
      if [ "$LOGNAME" = root ] || [ "$(id -u)" -eq 0 ]; then
      	PS1="\[\e[01;31m\]\[\u@\h:\w\n# \]\\[\e[00m\]"
      else
      	PS1="[\w]\n$ "
      fi
    '';
  };

  users = {
    mutableUsers = false;
    users = {
      root.hashedPasswordFile = "/persistent/var/keys/rootP";
      zenex = {
        hashedPasswordFile = "/persistent/var/keys/zenexP";
        shell = pkgs.zsh;
        isNormalUser = true;
        description = "zenex";
        extraGroups = [
          "seatd"
          "wheel"
          "video"
          "audio"
          "input"
          "libvirtd"
          "wireshark"
          "docker"
          "adbusers"
          "kvm"
        ];
      };
    };
  };

  console = {
    useXkbConfig = true;
    packages = [pkgs.uw-ttyp0];
    font = "t0-16-uni";
  };

  services = {
    tailscale = {
      enable = true;
      useRoutingFeatures = "both";
    };
    locate = {
      enable = true;
      package = pkgs.plocate;
      output = /var/cache/locate/locatedb;
      interval = "hourly";
      pruneNames = [".bzr" ".cache" ".git" ".hg" ".svn" ".ccls-cache" "*env*"];
    };
    #udev.packages = [
    #  pkgs.android-udev-rules
    #];
    seatd = {
      enable = true;
    };
    mullvad-vpn = {
      enable = true;
      package = pkgs.unstable.mullvad;
    };
    nscd.enableNsncd = true;
    gnome.gnome-keyring.enable = true;
    # journald.extraConfig = ''
    #   # SystemMaxUse=250M
    #   MaxRetentionSec=1month
    # '';
    logind = {lidSwitch = "suspend";};
    #with hardened profile this is needed otherwise nix will not build
    #    logrotate.checkConfig = false;
    fstrim.enable = true;
    openssh = {
      enable = true;
      settings.PasswordAuthentication = true;
      settings.KbdInteractiveAuthentication = true;
      settings.PermitRootLogin = "no";
      allowSFTP = false;
      extraConfig = ''
        AllowTcpForwarding yes
        X11Forwarding no
        AllowAgentForwarding no
        AllowStreamLocalForwarding no
      '';
    };

    printing.enable = false; #enable for printing
    # printing.drivers = [
    #   # pkgs.gutenprint
    #   pkgs.gutenprintBin
    #   pkgs.epson-escpr
    #   pkgs.epson-escpr2
    #   # pkgs.foomatic-db-ppds-withNonfreeDb
    #   #      pkgs.foomatic-db-nonfree
    # ];
    #avahi = { #needed for printing
    #  enable = true;
    #  nssmdns4 = true;
    #  openFirewall = true;
    #};
    dbus.enable = true;
    libinput.enable = true;
    xserver = {
      enable = true;
      windowManager.xmonad = {
        enable = false;
        enableContribAndExtras = false;
      };
      displayManager.startx.enable = true;
      xkb = {
        layout = "gb";
        variant = "";
        options = "altwin:ctrl_alt_win";
      };
    };
    usbguard = {
      enable = false;
      presentControllerPolicy = "apply-policy";
      implicitPolicyTarget = "block";
      rules = ''
        allow id 1d6b:0002 serial "0000:03:00.3" name "xHCI Host Controller" hash "cmzdizBgI3dt4cFMO8H1QUW3FWqsDSXqkGP5r5XZtEs=" parent-hash "PeOe/xaA1IiqkrGiI1Zaa9uUS35iLSvdBe78BridqEQ=" with-interface 09:00:00 with-connect-type ""
        allow id 1d6b:0003 serial "0000:03:00.3" name "xHCI Host Controller" hash "BJI8loU2ltTPxc6wSrxp3Rzpt/xpSwuuJ/+QTie7fnI=" parent-hash "PeOe/xaA1IiqkrGiI1Zaa9uUS35iLSvdBe78BridqEQ=" with-interface 09:00:00 with-connect-type ""
        allow id 0951:1666 serial "408D5C15CB92E911290E05C5" name "DataTraveler 3.0" hash "2EaYKuXgzTb9sCQUwJZvP9dxQb0D4AguKGMqigouD5M=" parent-hash "cmzdizBgI3dt4cFMO8H1QUW3FWqsDSXqkGP5r5XZtEs=" with-interface 08:06:50 with-connect-type "hotplug"
        allow id 05e3:0610 serial "" name "USB2.1 Hub" hash "CbRB9LX/JdGjNWCYSOcIwMVXE0UpOR03LCotWrTbuCM=" parent-hash "cmzdizBgI3dt4cFMO8H1QUW3FWqsDSXqkGP5r5XZtEs=" with-interface 09:00:00 with-connect-type "hotplug"
        allow id 05e3:0610 serial "" name "USB2.0 Hub" hash "To7KDzOAgi4jFrnLNIttvUKO428/MLM1/eWqsv969gw=" parent-hash "cmzdizBgI3dt4cFMO8H1QUW3FWqsDSXqkGP5r5XZtEs=" with-interface { 09:00:01 09:00:02 } with-connect-type "hardwired"
        allow id 05e3:0626 serial "" name "USB3.1 Hub" hash "15SBGsOo8K+JjtOKSCn7t0i6ifer4wmhzep1yEB5pLQ=" parent-hash "BJI8loU2ltTPxc6wSrxp3Rzpt/xpSwuuJ/+QTie7fnI=" with-interface 09:00:00 with-connect-type "hotplug"
        allow id 2972:0047 serial "" name "FiiO K3" hash "YNur060AyFfKrnIT2qfA+GDscg0vNchYtt0Lh3j2Zt4=" parent-hash "CbRB9LX/JdGjNWCYSOcIwMVXE0UpOR03LCotWrTbuCM=" with-interface { 01:01:20 01:02:20 01:02:20 01:02:20 01:02:20 fe:01:01 01:01:20 01:02:20 01:02:20 01:02:20 01:02:20 fe:01:01 } with-connect-type "unknown"
        allow id 046d:c08b serial "1285335A3232" name "G502 HERO Gaming Mouse" hash "ukNMlamAkPMh7baihqjodyq1X2cF75bqoMTP6vnHADw=" parent-hash "CbRB9LX/JdGjNWCYSOcIwMVXE0UpOR03LCotWrTbuCM=" with-interface { 03:01:02 03:00:00 } with-connect-type "unknown"
        allow id 0951:16dc serial "" name "HyperX Alloy FPS RGB" hash "H/mSsemErhu6jSIjbiA0PEz4NYHdH0Q5juMtNDc0eGA=" parent-hash "CbRB9LX/JdGjNWCYSOcIwMVXE0UpOR03LCotWrTbuCM=" with-interface { 03:01:01 03:01:01 03:00:00 } with-connect-type "unknown"
        allow id 5986:2137 serial "" name "Integrated Camera" hash "eg+SlU0pANNmAjsl8cDYiULjq9l+rGJ1kbvX/N+2r/Y=" parent-hash "To7KDzOAgi4jFrnLNIttvUKO428/MLM1/eWqsv969gw=" with-interface { 0e:01:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 } with-connect-type "hardwired"
        allow id 0bc2:2343 serial "NACAPZXW" name "Portable" hash "uNlV1Q6teT1NaR89ByP5xMO1US495x/RUOVCAStUXtA=" parent-hash "15SBGsOo8K+JjtOKSCn7t0i6ifer4wmhzep1yEB5pLQ=" with-interface { 08:06:50 08:06:62 } with-connect-type "unknown"
        allow id 04e8:61fb serial "S6YJNJ0X301164A" name "PSSD T7 Shield" hash "m0oln1SciQzP6k4TPn9ZFC03YkSkbwUSkNQCHS9PvTw=" parent-hash "15SBGsOo8K+JjtOKSCn7t0i6ifer4wmhzep1yEB5pLQ=" with-interface { 08:06:50 08:06:62 } with-connect-type "unknown"
        allow id 0bda:0177 serial "20121112761000000" name "USB2.0-CRW" hash "e2uNi2LwAzeDYUbSBzd8VS6LqfpmZj/vnXkKxI8Fa4c=" parent-hash "cmzdizBgI3dt4cFMO8H1QUW3FWqsDSXqkGP5r5XZtEs=" with-interface 08:06:50 with-connect-type "hardwired"
        allow id 16c0:27db serial "moergo.com:GLV80-4A2A8176E2496089" name "Glove80 Left" hash "aD39zz93dNP61EPXjHtlX+fg7EcNlc5a41B5pO+JDCU=" parent-hash "CbRB9LX/JdGjNWCYSOcIwMVXE0UpOR03LCotWrTbuCM=" with-interface 03:01:01 with-connect-type "unknown"
        allow id 090c:1000 serial "0320619110005669" name "Flash Drive" hash "HN91eZPfVx5LVYm22GQRriZM/HbCPF1fmILuq423EwQ=" parent-hash "15SBGsOo8K+JjtOKSCn7t0i6ifer4wmhzep1yEB5pLQ=" with-interface 08:06:50 with-connect-type "unknown"
        allow id 090c:1000 serial "0378623070002866" name "Flash Drive" hash "26QA1cD/Y0OQ39RG37alHNi4YKqSkA6hl+wiT+3SVzk=" parent-hash "15SBGsOo8K+JjtOKSCn7t0i6ifer4wmhzep1yEB5pLQ=" with-interface 08:06:50 with-connect-type "unknown"
      '';
    };
    opensnitch.enable = false;
    ntp.enable = false; # #disable the systemd-timesyncd
    chrony = {
      enable = true;
      initstepslew.enabled = true;
      serverOption = "iburst";
      enableNTS = true;
      #https://github.com/GrapheneOS/infrastructure/blob/8b87654075d954043d710596940426fb62b79ef9/etc/chrony.conf
      extraConfig = ''
        minsources 2
        authselectmode require

        # EF
        dscp 46

        leapsectz right/UTC
        makestep 1.0 3

        cmdport 0
      '';
    };

    thermald.enable = false;
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
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      wireplumber.enable = true;
    };
    gvfs.enable = false;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  fileSystems."/persistent".neededForBoot = true;
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
    defaultPackages = lib.mkForce [];
    systemPackages = with pkgs; [git vim emacs-nox tmux sbctl];
    pathsToLink = ["/share/bash-completion" "/share/zsh" "/share/xdg-desktop-portal" "/share/applications"];
    persistence."/persistent" = {
      enable = true; # NB: Defaults to true, not needed
      hideMounts = true;
      directories = [
        "/etc/nixos"
        "/var/log"
        "/etc/secureboot"
        "/var/lib/nixos"
        "/var/lib/systemd/coredump"
        "/var/lib/iwd/"
        "/etc/docker/key.json"
        "/var/lib/docker/"
        "/var/lib/lxd/"
        "/var/lib/libvirt/"
        "/var/cache/locate/"
        {
          directory = "/var/lib/tailscale";
          user = "root";
          group = "root";
          mode = "0700";
        }
        {
          directory = "/etc/mullvad-vpn";
          user = "root";
          group = "root";
          mode = "0755";
        }
        {
          directory = "/var/keys";
          user = "root";
          group = "root";
          mode = "0700";
        }
      ];

      users.zenex = {
        directories = [
          {
            directory = ".gnupg";
            mode = "0700";
          }
          {
            directory = ".ssh";
            mode = "0700";
          }
          {
            directory = ".local/state/syncthing";
            mode = "0700";
          }
          {
            directory = ".local/share/zoxide/";
            mode = "0700";
          }
        ];
      };
    };
  };

  fonts = {
    packages = with pkgs; [iosevka-bin vistafonts uw-ttyp0];
    fontconfig = {
      antialias = true;
      hinting.enable = true;
      hinting.style = "full";
      subpixel.rgba = "rgb";
    };
  };

  security = {
    pam.services.hyprlock = {};
    #    apparmor = {
    #      enable = true;
    #    };
    #    auditd.enable = true;
    #    audit.enable = true;
    #    audit.rules = ["-a exit,always -F arch=b64 -S execve"];
    chromiumSuidSandbox.enable = true;
    rtkit.enable = true;
    polkit.enable = true;
    # sudo = {
    #   execWheelOnly = true;
    #   extraConfig = ''
    #     Defaults lecture = never
    #   '';
    # };
    sudo-rs = {
      enable = true;
      execWheelOnly = true;
    };
  };

  networking.firewall = {
    #    nftables.enable = true;
    enable = true;
    #    pingLimit = "--limit 1/minute --limit-burst 5";
    # allowedTCPPorts = [631 5353]; # printing
    # allowedUDPPorts = [631 5353]; # printing

    allowedTCPPorts = [33573];
    allowedUDPPorts = [33573];

    # allowedTCPPortRanges = [
    #   {
    #     from = 1714;
    #     to = 1764;
    #   }
    #   # kdeconnect
    # ];
    # allowedUDPPortRanges = [
    #   {
    #     from = 1714;
    #     to = 1764;
    #   }
    #   # kdeconnect
    # ];
  };
  nix = {
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
    settings = {
      experimental-features = ["nix-command" "flakes"];
      auto-optimise-store = true;
      allowed-users = ["@wheel"];
      download-buffer-size = 524288000;
    };
    gc = {
      automatic = true;
      # runs twice a month on the 7th and the 21st
      dates = "*-*-7,21";
      options = "--delete-older-than 2d";
    };
  };

  system.stateVersion = "23.05"; # Did you read the comment?
}
