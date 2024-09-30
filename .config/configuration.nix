# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.supportedFilesystems = [ "ntfs" ];

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-bd718592-7be5-4ef0-b5ab-767ecd5ec66e".device = "/dev/disk/by-uuid/bd718592-7be5-4ef0-b5ab-767ecd5ec66e";
  boot.initrd.luks.devices."luks-bd718592-7be5-4ef0-b5ab-767ecd5ec66e".keyFile = "/crypto_keyfile.bin";

  networking.hostName = "flakes"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.utf8";

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
  users.users.zenex = {
    isNormalUser = true;
    description = "zenex";
    extraGroups = [ "networkmanager" "wheel" "video" "audio" ];
    packages = with pkgs; [];
  };

   programs.light.enable = true;

  #services.xserver.enable = true;
  #services.xserver.displayManager.startx.enable = true;
  #services.xserver.windowManager.xmonad = {
  #  enable = true;
  #  enableContribAndExtras = true;
  #};

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  neovim
  wget
  curl
  git
  firefox
  alacritty
  xfce.thunar
  cmus
  bashmount
  mupdf
  nnn
  xmobar
  htop
  feh
  playerctl
  redshift
  libreoffice
  dunst
  libnotify
  batsignal
  scrot
  mupdf
  bc
  zip
  unzip
  fuse3
  mpv
  keepassxc
  vimix-gtk-themes
  lxappearance
  dmenu
  xsecurelock
  alsa-utils
  lua
  ];

  environment.variables.EDITOR = "nvim";

  fonts.fonts = with pkgs; [
  (nerdfonts.override { fonts = [ "FiraMono"]; })
];

  security.doas.enable = true;
  security.doas.extraRules = [{
    users = [ "zenex" ];
    keepEnv = true;
    persist = true;
}];




  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
};

  nix.settings.auto-optimise-store = true;
  
  services.fstrim = {
  enable = true;
  interval = "weekly";
  };


  networking.firewall.enable = true;
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}