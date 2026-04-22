{
  inputs,
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [
    inputs.home-manager.nixosModules.home-manager
    inputs.disko.nixosModules.disko
    inputs.lanzaboote.nixosModules.lanzaboote
    #    ./hardened.nix
  ];
  zramSwap.enable = true;
  boot = {
    kernel.sysctl."vm.swappiness" = 0;
    # supportedFilesystems = ["ntfs"];
    # kernelPackages = pkgs.linuxPackages_latest;
    #loader.systemd-boot.enable = true;
    loader.systemd-boot.enable = lib.mkForce false;
    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };
    loader.efi.canTouchEfiVariables = true;
    # tmp.cleanOnBoot = true;
  };

  hardware = {
    amdgpu.opencl.enable = true;
    nitrokey.enable = true;
    bluetooth = {
      enable = false;
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
  users.extraGroups.vboxusers.members = ["zenex"];
  virtualisation = {
    libvirtd.enable = false;
    virtualbox.host.enable = true;
    virtualbox.host.enableHardening = false;
    # virtualbox.host.package = pkgs.unstable.virtualbox;
    docker = {
      enable = true;
      storageDriver = "btrfs";
      rootless.setSocketVariable = true;
    };
  };

  users = {
    users = {
      root = {
        shell = pkgs.bash;
      };
      zenex = {
        shell = pkgs.bash;
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

  powerManagement.powertop.enable = true;
  services = {
    smartd.enable = true;
    kmscon = {
      # broken doesn't launch guis https://github.com/NixOS/nixpkgs/issues/385497, unless the service is unstable(as of 02/26), as well as having the unstable package installed
      enable = true;
      hwRender = true;
      package = pkgs.unstable.kmscon;
      extraConfig = ''
        xkb-layout=gb
        xkb-options=altwin:ctrl_alt_win
        palette=custom
        palette-foreground=189,174,147
        palette-background=6,6,6
      '';
      fonts = [
        {
          name = "Iosevka";
          package = pkgs.iosevka;
        }
      ];
    };
    pcscd.enable = true;
    netbird = {
      enable = false;
      package = pkgs.unstable.netbird;
    };

    tailscale = {
      enable = false;
      useRoutingFeatures = "both";
    };
    udev.packages = [
      # pkgs.android-udev-rules
      pkgs.nitrokey-udev-rules
    ];
    seatd.enable = true;
    nscd.enableNsncd = true;
    gnome.gnome-keyring.enable = true;
    journald.extraConfig = ''
      #SystemMaxUse=250M
      MaxRetentionSec=1month
    '';
    #with hardened profile this is needed otherwise nix will not build
    #    logrotate.checkConfig = false;
    fstrim.enable = true;
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = true;
        PermitRootLogin = "no";
      };
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
    opensnitch.enable = false;
    ntp.enable = false; #disable the systemd-timesyncd
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
    gvfs.enable = false;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  security = {
    pam = {
      services.swaylock = {};
      u2f = {
        enable = true;
        settings = {
          authfile = "/home/zenex/Documents/u2f_keys";
          cue = true;
        };
      };
    };
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

  networking = {
    # nameservers = [
    #   "9.9.9.9"
    #   "149.112.112.112"
    #   "2620:fe::fe"
    #   "2620:fe::9"
    #   "1.1.1.1"
    # ];
    timeServers = [
      "time.cloudflare.com"
      "ntppool1.time.nl"
      "nts.netnod.se"
      "ptbtime1.ptb.de"
    ];
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
          # NameResolvingService = "none";
        };
      };
    };
    firewall = {
      #    nftables.enable = true;
      enable = false;
      #    pingLimit = "--limit 1/minute --limit-burst 5";
      # allowedTCPPorts = [631 5353]; # printing
      # allowedUDPPorts = [631 5353]; # printing

      allowedTCPPorts = [33573 6969];
      allowedUDPPorts = [33573 6969];

      #allowedTCPPortRanges = [
      #  {
      #    from = 1714;
      #    to = 1764;
      #  }
      #  # kdeconnect
      #];
      #allowedUDPPortRanges = [
      #  {
      #    from = 1714;
      #    to = 1764;
      #  }
      #  # kdeconnect
      #];
    };
  };
  nixpkgs = {
    # Configure your nixpkgs instance
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };
  nix = {
    package = pkgs.lix;
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
    registry.nixpkgs.flake = inputs.nixpkgs;
    settings = {
      experimental-features = ["nix-command" "flakes"];
      auto-optimise-store = true;
      allowed-users = ["@wheel"];
      trusted-users = config.nix.settings.allowed-users;
      # download-buffer-size = 524288000;
    };
    gc = {
      automatic = true;
      dates = "monthly";
      options = "--delete-older-than 2d";
    };
  };

  system.stateVersion = "23.05"; # Did you read the comment?
}
