# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ inputs, outputs, lib, config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    #./hardened.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    supportedFilesystems = [ "ntfs" ];
    tmp.cleanOnBoot = true;
    tmp.useTmpfs = true;
    initrd.luks.devices."luks-621f88b4-56cc-41aa-9c68-407f7664cc94".device = "/dev/disk/by-uuid/621f88b4-56cc-41aa-9c68-407f7664cc94";
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  networking = {
    hostName = "eukaryotic";
    networkmanager.enable = true;
    networkmanager.wifi.macAddress = "random";
    enableIPv6 = false;
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

  virtualisation.libvirtd.enable = true;
  programs = {
    firejail = { enable = true; };
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
    extraSpecialArgs = { inherit inputs outputs; };
    users = { zenex = import ../home-manager/home.nix; };
  };

  users.users.zenex = {
    isNormalUser = true;
    description = "zenex";
    extraGroups =
      [ "networkmanager" "wheel" "video" "audio" "input" "libvirtd" ];
    packages = with pkgs; [ ];
  };

  networking.timeServers = [
    "time.cloudflare.com"
    "ntppool1.time.nl"
    "nts.netnod.se"
    "ptbtime1.ptb.de"
  ];

  services = {
    #with hardened profile this is needed otherwise nix will not build
    #    logrotate.checkConfig = false;
    fwupd.enable = true;
        printing.enable = true;
        printing.drivers = [
          pkgs.gutenprint
          pkgs.gutenprintBin
          pkgs.epson-escpr
          pkgs.epson-escpr2
          pkgs.foomatic-db-ppds-withNonfreeDb
        ];
    avahi = {
      enable = true;
      nssmdns = true;
      openFirewall = true;
    };
    dbus.enable = true;
    smartd.enable = true;
    mysql = {
      enable = true;
      package = pkgs.mariadb;
    };
    xserver = {
      enable = true;
      displayManager.startx.enable = true;
      libinput.enable = true;
      layout = "gb";
      xkbVariant = "";
      xkb.options = "altwin:ctrl_alt_win";
    };
    usbguard.enable = true;
    #    usbguard.package = pkgs.usbguard-nox;
    usbguard.presentControllerPolicy = "apply-policy";
    usbguard.implicitPolicyTarget = "block";
    usbguard.rules = ''
      allow id 1d6b:0002 serial "0000:00:14.0" name "xHCI Host Controller" with-interface 09:00:00 with-connect-type ""
      allow id 1d6b:0003 serial "0000:00:14.0" name "xHCI Host Controller" with-interface 09:00:00 with-connect-type ""
      allow id 0951:16dc serial "" name "HyperX Alloy FPS RGB" with-interface { 03:01:01 03:01:01 03:00:00 } with-connect-type "hotplug"
      allow id 046d:c07e serial "497530453739" name "Gaming Mouse G402" with-interface { 03:01:02 03:00:00 } with-connect-type "hotplug"
      allow id 13d3:5248 serial "NULL" name "Integrated Camera" with-interface { 0e:01:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 } with-connect-type "not used"
      allow id 0bc2:2343 serial "NACAPZXW" name "Portable" with-interface { 08:06:50 08:06:62 } with-connect-type "hotplug"
      allow id 090c:1000 serial "0320619110005669" name "Flash Drive" with-interface 08:06:50 with-connect-type "hotplug"
      allow id 047d:1022 serial "" name "Kensington USB Orbit" with-interface 03:01:02 with-connect-type "hotplug"

    '';
    #    opensnitch.enable = true;
    ntp.enable = false;
    chrony = {
      enable = true;
      initstepslew.enabled = true;
      serverOption = "iburst";
      enableNTS = true;
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

    thermald.enable = true;
    tlp.enable = true;
    tlp.settings = {
      # CPU_SCALING_MIN_FREQ_ON_AC = 2800000;
      # CPU_SCALING_MAX_FREQ_ON_AC = 2800000;
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
    };
    gvfs.enable = true;
  };

  powerManagement.enable = true;
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [ ];
  environment.pathsToLink = [ "/share/zsh" "/share/bash-completion" ];
  fonts.packages = with pkgs; [
    iosevka
    vistafonts
    (nerdfonts.override { fonts = [ "Iosevka" "IosevkaTerm" ]; })
  ];

  security = {
    apparmor = { enable = true; };
    chromiumSuidSandbox.enable = true;
    rtkit.enable = true;
    polkit.enable = true;
  };

  networking.firewall = {
    enable = true;

    allowedTCPPorts = [ 631 ]; # printing
    allowedUDPPorts = [ 631 ]; # printing

    allowedTCPPortRanges = [{
      from = 1714;
      to = 1764;
    } # kdeconnect
      ];
    allowedUDPPortRanges = [{
      from = 1714;
      to = 1764;
    } # kdeconnect
      ];
  };

  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
