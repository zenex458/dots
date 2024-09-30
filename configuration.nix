# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./hardened.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.tmp.cleanOnBoot = true;
  boot.tmp.useTmpfs = true;

  boot.initrd.luks.devices."luks-a71f7169-aa2c-4f1a-86b5-1818f35c5b47".device = "/dev/disk/by-uuid/a71f7169-aa2c-4f1a-86b5-1818f35c5b47";
  networking.hostName = "flakes"; # Define your hostname.
  # Enable networking
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.macAddress = "random";

  networking.enableIPv6 = false;
  hardware.cpu.intel.updateMicrocode = true;

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Select internationalisation properties.
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

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    displayManager.startx.enable = true;
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    libinput.enable = true;
    layout = "gb";
    xkbVariant = "";
  };

  # Configure console keymap
  console.keyMap = "uk";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  programs.dconf.enable = true;
  programs.bash.undistractMe.timeout = 30;
  programs.bash.undistractMe.playSound = true;
  programs.bash.promptInit =
    ''
      if [ "$LOGNAME" = root ] || [ "$(id -u)" -eq 0 ]; then
      	PS1="\[\e[01;31m\]\[\u@\h:\w# \]\\[\e[00m\]"
      else
      	PS1="[\w]\nλ "
      fi
    '';
  programs.bash.interactiveShellInit =
    ''
      [ -f "$HOME/.config/.aliasrc" ] && source "$HOME/.config/.aliasrc"
      [ -f "$HOME/.config/.envrc" ] && source "$HOME/.config/.envrc"
      shopt -s autocd cdspell
      HISTFILE="$HOME/.local/share/.bash_history"
      HISTSIZE=1000
      HISTFILESIZE=2000
      set -o emacs
      bind 'set show-all-if-ambiguous on'
      bind 'set completion-ignore-case on'
      bind 'TAB:menu-complete'
    '';
  users.users.zenex = {
    isNormalUser = true;
    description = "zenex";
    extraGroups = [ "networkmanager" "wheel" "video" "audio" ];
    packages = with pkgs; [
      neovim
      git
      htop
      ffmpeg
      xterm
      rxvt-unicode-unwrapped
      tmux
      nnn
      xmobar
      trash-cli
      firefox
      dmenu
      feh
      redshift
      libreoffice
      dunst
      libnotify
      scrot
      mupdf
      zathura
      bc
      p7zip
      zip
      unzip
      fuse3
      mpv
      keepassxc
      lxappearance
      xsecurelock
      alsa-utils
      pulsemixer
      z-lua
      shellcheck
      yt-dlp
      kdeconnect
      gcc
      astyle
      nixpkgs-fmt
      ormolu
      shfmt
      xautolock
      mpd
      mpc-cli
      ncmpcpp
      xclip
      xsel
      ghc
      pandoc
      obsidian
      lxqt.lxqt-policykit
      texlive.combined.scheme-full
      paper-icon-theme
      shades-of-gray-theme
      xfce.thunar
      fd
      ripgrep


      (aspellWithDicts (dicts: with dicts; [ en en-computers en-science uk ]))

    ];
  };

  services.usbguard.enable = true;
  services.usbguard.package = pkgs.usbguard-nox;
  services.usbguard.presentControllerPolicy = "apply-policy";
  services.usbguard.implictPolicyTarget = "reject";
  services.usbguard.rules =
    ''
      allow id 1d6b:0002 serial "0000:00:14.0" name "xHCI Host Controller" with-interface 09:00:00 with-connect-type ""
      allow id 1d6b:0003 serial "0000:00:14.0" name "xHCI Host Controller" with-interface 09:00:00 with-connect-type ""
      allow id 0951:16dc serial "" name "HyperX Alloy FPS RGB" with-interface { 03:01:01 03:01:01 03:00:00 } with-connect-type "hotplug"
      allow id 046d:c07e serial "497530453739" name "Gaming Mouse G402" with-interface { 03:01:02 03:00:00 } with-connect-type "hotplug"
      allow id 13d3:5248 serial "NULL" name "Integrated Camera" with-interface { 0e:01:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 } with-connect-type "not used"

    '';

  services.emacs = {
    enable = true;
    package = pkgs.emacs29-gtk3;
  };

  services.ntp.enable = false;
  services.chrony.enable = true;
  services.chrony.initstepslew.enabled = true;
  services.chrony.serverOption = "iburst";
  services.chrony.enableNTS = true;
  programs.light.enable = true;
  services.thermald.enable = true;
  powerManagement.enable = true;
  services.tlp.enable = true;
  services.tlp.settings = {
    CPU_SCALING_MIN_FREQ_ON_AC = 400000;
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

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    #  wget
  ];

  environment.variables.EDITOR = "nvim";

  fonts.fonts = with pkgs; [
    iosevka
    vistafonts
  ];

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };


  security.polkit.enable = true;

  nix.settings.auto-optimise-store = true;

  services.gvfs.enable = true;

  networking.firewall = {
    enable = true;
    #   allowedUDPPorts = [
    #   42000
    #   42001
    #   ];
    #   allowedTCPPorts = [
    #   42000
    #   42001
    #   ];
    allowedTCPPortRanges = [
      { from = 1714; to = 1764; }
    ];
    allowedUDPPortRanges = [
      { from = 1714; to = 1764; }
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}



