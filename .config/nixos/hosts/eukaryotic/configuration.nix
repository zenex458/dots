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
    #    ./hardened.nix
    ./disko-config.nix
    ./hardware-configuration.nix
    ./pkgs.nix
  ];
  boot = {
    # kernel.sysctl."vm.swappiness" = 30;
    supportedFilesystems = ["ntfs"];
    kernelPackages = pkgs.linuxPackages_latest;
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
    hostName = "eukaryotic";
    wireless.iwd = {
      enable = true;
      settings = {
        General = {
          "EnableNetworkConfiguration" = true;
          "AddressRandomization" = "network";
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
    cpu.amd.updateMicrocode = true;
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

  programs = {
    obs-studio.enable = true;
    niri.enable = true;
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
    # udev.packages = [
    #   pkgs.android-udev-rules
    # ];
    seatd = {
      enable = true;
    };
    mullvad-vpn.enable = true;
    mullvad-vpn.package = pkgs.mullvad-vpn;
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
      # require public key authentication for better security
      settings.PasswordAuthentication = false;
      settings.KbdInteractiveAuthentication = false;
      settings.PermitRootLogin = "no";
      allowSFTP = false;
      extraConfig = ''
        AllowTcpForwarding yes
        X11Forwarding no
        AllowAgentForwarding no
        AllowStreamLocalForwarding no
        #AuthenticationMethods publickey
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
    # avahi = { #enable for printing
    #   enable = true;
    #   nssmdns4 = true;
    #   openFirewall = true;
    # };
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
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
        CPU_MIN_PERF_ON_AC = 0;
        CPU_MAX_PERF_ON_AC = 100;
        CPU_MIN_PERF_ON_BAT = 0;
        CPU_MAX_PERF_ON_BAT = 50;
        CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";
        START_CHARGE_THRESH_BAT0 = 75;
        # STOP_CHARGE_THRESH_BAT0 = 80;
        STOP_CHARGE_THRESH_BAT0 = 1; #for on, for some reason. run `sudo tlp-stat' to see what value you should use
      };
    };
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      wireplumber.enable = true;
    };
    # gvfs.enable = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  fileSystems."/persistent".neededForBoot = true;
  environment = {
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
      # FREETYPE_PROPERTIES = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
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
    pathsToLink = ["/share/bash-completion" "/share/zsh"];
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
    # fontconfig = {
    #   antialias = true;
    #   hinting.enable = true;
    #   hinting.style = "full";
    #   subpixel.rgba = "rgb";
    # };
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
      dates = "daily";
      options = "--delete-older-than 2d";
    };
  };

  system.stateVersion = "23.05"; # Did you read the comment?
}
