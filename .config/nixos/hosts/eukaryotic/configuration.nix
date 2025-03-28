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
    inputs.hosts.nixosModule
    #    ./hardened.nix
    ./disko-config.nix
    ./hardware-configuration.nix
  ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.loader.systemd-boot.enable = lib.mkForce false;
  # boot.loader.systemd-boot.enable = true;
  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/etc/secureboot";
  };
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmp.cleanOnBoot = true;

  boot.initrd.postDeviceCommands = lib.mkAfter ''
    mkdir /btrfs_tmp
    mount -o subvol=/ /dev/mapper/crypted /btrfs_tmp
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

  systemd.tmpfiles.rules = [
    "d /persistence/home/ 1777 root root-"
#    "d /persistence/home/zenex/ 0770 zenex users-"
    "d /persistence/home/zenex/ 1777 zenex users-"
    "d /persistent/var/keys/ 0600 root root-"
#    "f /etc/mullvad-vpn/device.json 0600 root root-"
#    "f /etc/mullvad-vpn/settings.json 0644 root root-"
#    "f /etc/mullvad-vpn/account-history.json 0644 root root-"
#    "L /var/lib/lxd - - - - /persist/var/lib/lxd"
#    "L /var/lib/docker - - - - /persist/var/lib/docker"
  ];

  programs.fuse.userAllowOther = true;
  home-manager = {
    extraSpecialArgs = {inherit inputs;};
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
          # "AddressRandomization" = "network";
          # "AddressRandomizationRange" = "nic";
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

  hardware.graphics.enable = true;

  hardware.cpu.intel.updateMicrocode = true;
  time.timeZone = "Europe/London";

  i18n.defaultLocale = "en_GB.UTF-8";

  i18n.extraLocaleSettings = {
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

  virtualisation = {
    libvirtd.enable = true;
    docker = {
      enable = true;
      storageDriver = "btrfs";
      rootless.setSocketVariable = true;
    };
  };

  programs = {
    river = {
      enable = true;
      xwayland.enable = true;
    };
    ssh.startAgent = true;
    gnupg.agent.enable = true;
    wireshark = {
      enable = true;
      package = pkgs.wireshark;
    };
    xwayland.enable = true;
    hyprland = {
      enable = false;
      withUWSM = true;
      xwayland.enable = true;
    };
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
    dconf.enable = true;
    zsh = {enable = true;}; ##add some more stuff for root
    bash.promptInit = ''
      if [ "$LOGNAME" = root ] || [ "$(id -u)" -eq 0 ]; then
      	PS1="\[\e[01;31m\]\[\u@\h:\w\n# \]\\[\e[00m\]"
      else
      	PS1="[\w]\n$ "
      fi
    '';
  };

  users.mutableUsers = false;
  users.users.root.hashedPasswordFile = "/persistent/var/keys/rootP";
  programs.adb.enable = true;
  users.users.zenex = {
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

  console = {
    useXkbConfig = true;
    font = "Lat2-Terminus16";
  };

  services = {
    seatd = {
      enable = true;
    };
   # mullvad-vpn.enable = true;
   # mullvad-vpn.package = pkgs.mullvad-vpn;
    nscd.enableNsncd = true;
    gnome.gnome-keyring.enable = true;
    journald.extraConfig = ''
      # SystemMaxUse=250M
      MaxRetentionSec=1month
    '';
    logind = {lidSwitch = "suspend";};
    #irqbalance.enable = true;
    #with hardened profile this is needed otherwise nix will not build
    #    logrotate.checkConfig = false;
    #    fwupd.enable = true;
    fstrim.enable = true;

    openssh = {
      enable = true;
      # require public key authentication for better security
      # settings.PasswordAuthentication = false;
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
    printing.drivers = [
      # pkgs.gutenprint
      pkgs.gutenprintBin
      pkgs.epson-escpr
      pkgs.epson-escpr2
      # pkgs.foomatic-db-ppds-withNonfreeDb
      #      pkgs.foomatic-db-nonfree
    ];
    # avahi = { #enable for printing
    #   enable = true;
    #   nssmdns4 = true;
    #   openFirewall = true;
    # };
    dbus.enable = true;
    smartd.enable = true;
    # mysql = {
    #   enable = true;
    #   package = pkgs.mariadb;
    # };
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
    usbguard.enable = true;
    #    usbguard.package = pkgs.usbguard-nox;
    usbguard.presentControllerPolicy = "apply-policy";
    usbguard.implicitPolicyTarget = "block";
    usbguard.rules = ''
      allow id 1d6b:0002 serial "0000:00:14.0" name "xHCI Host Controller" hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" parent-hash "rV9bfLq7c2eA4tYjVjwO4bxhm+y6GgZpl9J60L0fBkY=" with-interface 09:00:00 with-connect-type ""
      allow id 1d6b:0003 serial "0000:00:14.0" name "xHCI Host Controller" hash "3Wo3XWDgen1hD5xM3PSNl3P98kLp1RUTgGQ5HSxtf8k=" parent-hash "rV9bfLq7c2eA4tYjVjwO4bxhm+y6GgZpl9J60L0fBkY=" with-interface 09:00:00 with-connect-type ""
      allow id 13d3:5248 serial "NULL" name "Integrated Camera" hash "vOb1VgH5t/BCYgTlNwrs6lWKhJmuwyvRE9dEeCJLIDY=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" with-interface { 0e:01:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 } with-connect-type "not used"
      allow id 0bc2:2343 serial "NACAPZXW" name "Portable" hash "uNlV1Q6teT1NaR89ByP5xMO1US495x/RUOVCAStUXtA=" parent-hash "3Wo3XWDgen1hD5xM3PSNl3P98kLp1RUTgGQ5HSxtf8k=" with-interface { 08:06:50 08:06:62 } with-connect-type "hotplug"
      allow id 090c:1000 serial "0378623070002866" name "Flash Drive" hash "26QA1cD/Y0OQ39RG37alHNi4YKqSkA6hl+wiT+3SVzk=" parent-hash "3Wo3XWDgen1hD5xM3PSNl3P98kLp1RUTgGQ5HSxtf8k=" with-interface 08:06:50 with-connect-type "hotplug"
      allow id 04e8:61fb serial "S6YJNJ0X301164A" name "PSSD T7 Shield" hash "m0oln1SciQzP6k4TPn9ZFC03YkSkbwUSkNQCHS9PvTw=" parent-hash "3Wo3XWDgen1hD5xM3PSNl3P98kLp1RUTgGQ5HSxtf8k=" with-interface { 08:06:50 08:06:62 } with-connect-type "hotplug"
      allow id 0781:5575 serial "04020130092220073710" name "Cruzer Glide" hash "q2jgfmoO2UHR5ZeeiwhacPfiBGPwGs+GCzv9vYk5efA=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" with-interface 08:06:50 with-connect-type "hotplug"
      allow id 090c:1000 serial "0320619110005669" name "Flash Drive" hash "HN91eZPfVx5LVYm22GQRriZM/HbCPF1fmILuq423EwQ=" parent-hash "3Wo3XWDgen1hD5xM3PSNl3P98kLp1RUTgGQ5HSxtf8k=" with-interface 08:06:50 with-connect-type "hotplug"
      allow id 0951:16dc serial "" name "HyperX Alloy FPS RGB" hash "H/mSsemErhu6jSIjbiA0PEz4NYHdH0Q5juMtNDc0eGA=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" with-interface { 03:01:01 03:01:01 03:00:00 } with-connect-type "hotplug"
      allow id 046d:c08b serial "1285335A3232" name "G502 HERO Gaming Mouse" hash "ukNMlamAkPMh7baihqjodyq1X2cF75bqoMTP6vnHADw=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" with-interface { 03:01:02 03:00:00 } with-connect-type "hotplug"
      allow id 22d9:2769 serial "2d3a83b0" name "OnePlus NordCE 5G" hash "otW4Gq81kNDNk0jPk176y7iqEp56g4nLwM4sVQiqq2M=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" with-interface ff:42:01 with-connect-type "hotplug"
      allow id 090c:1000 serial "0378623070002866" name "Flash Drive" hash "1/RruVSzVscnzrC1F2G9vbXXh5TSrNFf7UZ3aEBxar8=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" with-interface 08:06:50 with-connect-type "hotplug"
      allow id 0781:5583 serial "4C531001400313104105" name "Ultra Fit" hash "FIp1dacmLw5lMGjbAgLw//HPgWhrQDdDm9jRfpSr69Y=" parent-hash "3Wo3XWDgen1hD5xM3PSNl3P98kLp1RUTgGQ5HSxtf8k=" with-interface 08:06:50 with-connect-type "hotplug"
      allow id 1038:184c serial "" name "SteelSeries Rival 3" hash "sYta/t0uxVWU/6EekbEp2yRDxjGzoHfZ9UK4M3wxVn4=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" via-port "1-6" with-interface { 03:01:02 03:00:00 03:00:00 03:00:00 } with-connect-type "hotplug"
      allow id 0951:1666 serial "E0D55EA5741DE56049190FED" name "DataTraveler 3.0" hash "7jEH3bcZPd4Ee2CA2Ggd7BHeXMjSWmQ1UX1P45KA/TQ=" parent-hash "3Wo3XWDgen1hD5xM3PSNl3P98kLp1RUTgGQ5HSxtf8k=" with-interface 08:06:50 with-connect-type "hotplug"
      allow id 058f:6387 serial "041EBAED8B84A741" name "Disk 2.0" hash "yszYXNmkr8aH7Pb+Bam3eHnkPJanUuV3VpMypoaYhhU=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" with-interface 08:06:50 with-connect-type "hotplug"
    '';
    # opensnitch.enable = true;
    ntp.enable = false; # #disable the systemd-timesyncd
    chrony = {
      enable = true;
      initstepslew.enabled = true;
      serverOption = "iburst";
      enableNTS = true;
      #https://raw.githubusercontent.com/GrapheneOS/infrastructure/main/chrony.conf
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
    tlp.enable = true;
    tlp.settings = {
      CPU_SCALING_MIN_FREQ_ON_AC = 2800000;
      CPU_SCALING_MAX_FREQ_ON_AC = 2800000;
      CPU_SCALING_MIN_FREQ_ON_BAT = 400000;
      CPU_SCALING_MAX_FREQ_ON_BAT = 2000000;
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 50;
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";
    };
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      wireplumber.enable = true;
      ##      extraConfig.pipewire."92-low-latency" = {
      ##        "context.properties" = {
      ##          "default.clock.rate" = 48000;
      ##          "default.clock.quantum" = 32;
      ##          "default.clock.min-quantum" = 32;
      ##          "default.clock.max-quantum" = 32;
      ##        };
      ##      };
    };
    gvfs.enable = true;
  };

  nixpkgs.config = {allowUnfree = true;};

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  fileSystems."/persistent".neededForBoot = true;
  environment = {
    sessionVariables.NIXOS_OZONE_WL = "1";
    sessionVariables.FREETYPE_PROPERTIES = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
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
        private-bin dbus-launch,dbus-send,firefox,which
      '';
      "firejail/firefox-common.local".text = ''
        private-etc fonts,group,hosts,localtime,nsswitch.conf,pki,pulse,resolv.conf,ssl
        private-tmp
      '';
      # "firejail/nolocal.net" = {
      #   source = "${pkgs.firejail}/etc/firejail/nolocal.net";
      #   mode = "0644";
      # };
    };
    defaultPackages = lib.mkForce [];
    systemPackages = with pkgs; [git neovim emacs-nox];
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
        "/var/lib/libvirt/images"
#        {
#          directory = "/etc/mullvad-vpn";
#          user = "root";
#          group = "root";
#          mode = "0755";
#        }
        {
          directory = "/var/keys";
          user = "root";
          group = "root";
          mode = "0700";
        }
      ];
#      files = [
#        "/etc/machine-id"
#        "/etc/mullvad-vpn/account-history.json"
#        "/etc/mullvad-vpn/device.json"
#        "/etc/mullvad-vpn/settings.json"
#      ];
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
        ];
      };
    };
  };

  fonts.packages = with pkgs; [iosevka-bin uw-ttyp0 vistafonts];

  fonts.fontconfig = {
    antialias = true;
    hinting.enable = true;
    hinting.style = "slight";
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
    sudo = {
      execWheelOnly = true;
      extraConfig = ''
        Defaults lecture = never
      '';
    };
  };

  networking.stevenBlackHosts = {
    enable = true;
    enableIPv6 = true;
    blockGambling = true;
    blockPorn = true;
  };

  networking.firewall = {
    #    nftables.enable = true;
    enable = true;
    #    pingLimit = "--limit 1/minute --limit-burst 5";
    # allowedTCPPorts = [631 5353]; # printing
    # allowedUDPPorts = [631 5353]; # printing

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

  systemd.services = {
#    nix-daemon = {
#      environment.TMPDIR = "/run/nixos";
#    };
    docker.serviceConfig = {
      NoNewPrivileges = true;
      ProtectSystem = "full";
      ProtectHome = true;
      ProtectKernelModules = true;
      ProtectKernelLogs = true;
      ProtectControlGroups = true;
      ProtectClock = true;
      ProtectProc = "invisible";
      PrivateTmp = true;
      PrivateMounts = true;
      RestrictRealtime = true;
      RestrictAddressFamilies = [
        "AF_UNIX"
        "AF_NETLINK"
        "AF_INET"
        "AF_INET6"
      ];
      RestrictNamespaces = [
        "~user"
      ];
      MemoryDenyWriteExecute = true;
      SystemCallFilter = [
        "~@debug"
        "~@raw-io"
        "~@reboot"
        "~@clock"
        "~@module"
        "~@swap"
        "~@obsolete"
        "~@cpu-emulation"
      ];
      SystemCallArchitectures = "native";
      CapabilityBoundingSet = [
        "~CAP_SYS_RAWIO"
        "~CAP_SYS_PTRACE"
        "~CAP_SYS_BOOT"
      ];
    };
  };

  nix = {
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
    settings = {
      experimental-features = ["nix-command" "flakes"];
      auto-optimise-store = true;
      allowed-users = ["@wheel"];
    };
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 2d";
    };
  };

  system.stateVersion = "23.05"; # Did you read the comment?
}
