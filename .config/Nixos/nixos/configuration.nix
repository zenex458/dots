# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  inputs,
  outputs,
  lib,
  #config,
  pkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    #./hardened.nix ##this breaks
    inputs.home-manager.nixosModules.home-manager
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = ["ntfs"];
    loader = {
      systemd-boot.enable = true;
      systemd-boot.editor = false;
      efi.canTouchEfiVariables = true;
    };
    # kernelParams = [
    #   "quiet"
    #   "splash"
    #   ############## i have now idea if the below works
    #   ###"vga=current"
    #   ###"rd.udev.log_level=3"
    #   ###"udev.log_priority=3"
    # ];
    ##consoleLogLevel = 0;
    ##initrd.verbose = false;
    #tmp.cleanOnBoot = true;
    tmp.useTmpfs = true;
    initrd.luks.devices."luks-c747cca5-93f7-40d7-836c-ef71a364d53b" = {
      device = "/dev/disk/by-uuid/c747cca5-93f7-40d7-836c-ef71a364d53b";
      allowDiscards = true;
    };
  };

  console = {
    useXkbConfig = true;
    font = "Lat2-Terminus16";
  };
  #TODO: https://wiki.nixos.org/wiki/Systemd/Hardening
  # https://www.opensourcerers.org/2022/04/25/optimizing-a-systemd-service-for-security/
  # https://github.com/alegrey91/systemd-service-hardening
  # https://blog.sergeantbiggs.net/posts/hardening-applications-with-systemd/
  # https://www.linuxjournal.com/content/systemd-service-strengthening
  # systemd.services = {
  #   macrand = {
  #     wantedBy = [ "multi-user.target" ];
  #     after = [ "network.target" ];
  #     description = "Randomise MAC address";
  #     serviceConfig = {
  #       Type = "oneshot";
  #       ExecStart = ''${pkgs.macchanger}/bin/macchanger -e wlp4s0'';
  #     };
  #   };
  #   nix-daemon = {
  #     environment.TMPDIR = "/tmp";
  #   };
  # };
  hardware.graphics = {
    enable = true;
  };

  networking = {
    hostName = "eukaryotic";
    networkmanager = {
      wifi.macAddress = "stable-ssid";
      wifi.scanRandMacAddress = true;
      enable = true;
    };
  };

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
      rootless.setSocketVariable = true;
    };
  };
  programs = {
    nh = {
      enable = true;
      clean = {
        enable = true;
        extraArgs = "--keep-since 2d";
      };
      flake = "/home/zenex/dots/.config/Nixos/";
    };
    gnupg.agent.enable = true;
    wireshark = {
      enable = true;
      package = pkgs.wireshark;
    };
    hyprland = {
      enable = true;
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
    xwayland.enable = true;
    light.enable = true;
    dconf.enable = true;
    bash.undistractMe.timeout = 30;
    bash.undistractMe.playSound = true;
    bash.promptInit = ''
      if [ "$LOGNAME" = root ] || [ "$(id -u)" -eq 0 ]; then
      	PS1="\[\e[01;31m\]\[\u@\h:\w# \]\\[\e[00m\]"
      else
      	PS1="[\w]\n$ "
      fi
    '';
  };

  home-manager = {
    extraSpecialArgs = {
      inherit inputs outputs;
    };
    users = {
      zenex = import ../home-manager/home.nix;
    };
  };

  users.users.zenex = {
    isNormalUser = true;
    description = "zenex";
    extraGroups = [
      "wheel"
      "video"
      "audio"
      "input"
      "libvirtd"
      "wireshark"
      "docker"
      "networkmanager"
    ];
    packages = with pkgs; [
    ];
  };

  #https://en.wikipedia.org/wiki/Network_Time_Protocol#Secure_extensions servers
  networking.timeServers = [
    "time.cloudflare.com"
    "ntppool1.time.nl"
    "nts.netnod.se"
    "ptbtime1.ptb.de"
  ];

  services = {
    gnome.gnome-keyring.enable = true;
    journald.extraConfig = "SystemMaxUse=250M\n";
    logind = {
      lidSwitch = "suspend";
    };
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
        AuthenticationMethods publickey
      '';
    };

    printing.enable = true;
    printing.drivers = [
      # pkgs.gutenprint
      pkgs.gutenprintBin
      pkgs.epson-escpr
      pkgs.epson-escpr2
      # pkgs.foomatic-db-ppds-withNonfreeDb
      pkgs.foomatic-db-nonfree
    ];
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
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
        enable = true;
        enableContribAndExtras = true;
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

  powerManagement.enable = true;
  nixpkgs.config = {
    allowUnfree = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    sessionVariables.NIXOS_OZONE_WL = "1";
    sessionVariables.FREETYPE_PROPERTIES = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
    #defaultPackages = lib.mkForce [ ];
    systemPackages = with pkgs; [
      git
      vim
    ];
    pathsToLink = [
      "/share/zsh"
      "/share/bash-completion"
    ];
    etc = {
      "firejail/firefox.local".text = ''
        # Enable native notifications.
        dbus-user.talk org.freedesktop.Notifications
        # Allow inhibiting screensavers.
        dbus-user.talk org.freedesktop.ScreenSaver
        # Allow screensharing under Wayland.
        dbus-user.talk org.freedesktop.portal.Desktop
        disable-mnt
        #restrict-namespaces ##understand this more
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
  };
  fonts.packages = with pkgs; [
    iosevka-bin
    uw-ttyp0
    vistafonts
    #    (nerdfonts.override { fonts = [ "Iosevka" "IosevkaTerm" ]; })
  ];

  fonts.fontconfig = {
    antialias = true;
    hinting.enable = true;
    hinting.style = "slight";
  };

  security = {
    pam.services.hyprlock = {};
    apparmor = {
      enable = true;
    };
    auditd.enable = true;
    audit.enable = true;
    audit.rules = ["-a exit,always -F arch=b64 -S execve"];
    chromiumSuidSandbox.enable = true;
    rtkit.enable = true;
    polkit.enable = true;
    sudo.execWheelOnly = true;
  };
  networking.firewall = {
    #    nftables.enable = true;
    enable = true;
    #    pingLimit = "--limit 1/minute --limit-burst 5";
    allowedTCPPorts = [
      631
      5353
    ]; # printing
    allowedUDPPorts = [
      631
      5353
    ]; # printing

    allowedTCPPortRanges = [
      {
        from = 1714;
        to = 1764;
      }
      # kdeconnect
    ];
    allowedUDPPortRanges = [
      {
        from = 1714;
        to = 1764;
      }
      # kdeconnect
    ];
  };

  nix = {
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      auto-optimise-store = true;
      allowed-users = ["@wheel"];
    };
    # gc = {
    #   automatic = true;
    #   dates = "19:00";
    #   options = "--delete-older-than 7d";
    # };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
