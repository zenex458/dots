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
  boot.cleanTmpDir = true;


  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-313e6e86-8932-4563-a228-eb5963c82498".device = "/dev/disk/by-uuid/313e6e86-8932-4563-a228-eb5963c82498";
  boot.initrd.luks.devices."luks-313e6e86-8932-4563-a228-eb5963c82498".keyFile = "/crypto_keyfile.bin";

  networking.hostName = "flakes"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;
  networking.enableIPv6 = false;

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
   # windowManager.dwm.enable = true;
    libinput.enable = true;
    layout = "gb";
    xkbVariant = "";
  };

  # Configure console keymap
  console.keyMap = "uk";

 #  virtualisation.libvirtd.enable = true;
 #  programs.dconf.enable = true;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.zenex = {
    isNormalUser = true;
    description = "zenex";
    extraGroups = [ "networkmanager" "wheel" "video" "audio" ]; #libvirtd
    packages = with pkgs; [];
  };

  programs.light.enable = true;
  services.tlp.enable = true;
  services.tlp.settings = {
    CPU_SCALING_GOVERNOR_ON_BAT="powersave";
    CPU_SCALING_GOVERNOR_ON_AC="performance";
    CPU_SCALING_MIN_FREQ_ON_AC=0;
    CPU_SCALING_MAX_FREQ_ON_AC=24000000;
    CPU_SCALING_MIN_FREQ_ON_BAT=0;
    CPU_SCALING_MAX_FREQ_ON_BAT=20000000;


  };

  nixpkgs.config.allowUnfree = true;
  #users.extraGroups.vboxusers.members = [ "zenex" ];
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  #shotcut
  #ffmpeg
  #gimp
  neovim
  vimv
  trash-cli
  aria
  nvi
  wget
  curl
  git
  firefox
  alacritty
  pcmanfm
  bashmount
  nnn
  xmobar
  htop
  feh
  redshift
  clifm
  libreoffice
  dunst
  libnotify
  #batsignal
  scrot
  #maim
  xdotool
  mupdf
  #imagemagick
  bc
  zip
  unzip
  fuse3
  mpv
  mpg123
  keepassxc
  paper-icon-theme
  orchis-theme
  lxappearance
  dmenu
  xsecurelock
  alsa-utils
  lua
  #zoxide
  fzf
  shellcheck
  rsync
  fzf-obc
  yt-dlp
  freetube
  oneshot
  #syncthing
  kdeconnect
  gcc
  #arandr
  #sdcv
  tmux
  microcodeIntel
  mpd
  mpc-cli
  ncmpcpp
  pavucontrol
  xclip
  xsel
  #httrack
  #sbcl
  #nodejs
  ghc
  haskell-language-server
  #android-tools
  #android-file-transfer
  #usbutils
  mono
  omnisharp-roslyn
  dotnet-sdk
  dotnetPackages.Nuget
  #dotnet-aspnetcore
  #dotnet-runtime
  #vscode-with-extensions
  #vscode-extensions.ms-dotnettools.csharp
  #openssh
  lxqt.lxqt-policykit
  dbus
  #cpu-x
  ##selenium packages
  #python39Packages.selenium
  #chromedriver
  #google-chrome
  #dotnetPackages.Nuget
  #python3Full
  #python39Packages.pip
  #python.pkgs.pip
  ##end selenium packages
  #virt-manager
#    (st.overrideAttrs (oldAttrs: rec {
#    patches = [
##    /home/zenex/.config/st/st-blinking_cursor.diff
#    /home/zenex/.config/st/st-bold-is-not-bright.diff
#    ];
#    configFile = writeText "config.def.h" (builtins.readFile /home/zenex/.config/st/config.h);
#    postPatch = "${oldAttrs.postPatch}\n cp ${configFile} config.def.h";
#  }))
#
  ];

# nixpkgs.overlays = [
#    (final: prev: {
#      dwm = prev.dwm.overrideAttrs (old: { src = /home/zenex/.config/dwm ;});
#    })
#];

#  services.openssh = {
#  enable = true;
#  #passwordAuthentication = false;
#  #kbdInteractiveAuthentication = false;
#  };

  environment.variables.EDITOR = "nvim";

#  fonts.fonts = with pkgs; [
#  (nerdfonts.override { fonts = [ "FiraMono"]; })
#  vistafonts
#];

  fonts.fonts = with pkgs; [
  hack-font
  inconsolata
  vistafonts
];

  security.polkit.enable = true;

  security.doas.enable = true;
  security.doas.extraRules = [{
    users = [ "zenex" ];
    keepEnv = true;
    persist = true;
}];

   hardware.pulseaudio.enable = true;
   hardware.pulseaudio.support32Bit = true;

  nix.settings.auto-optimise-store = true;
  
  #hardware.bluetooth.enable = true;
  #services.blueman.enable = true;

  services.gvfs.enable = true;
  
  services.fstrim = {
  enable = true;
  interval = "weekly";
  };


#  networking.extraHosts = let
#    hostsPath = https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn-social/hosts;
#    hostsFile = builtins.fetchurl hostsPath;
#  in builtins.readFile "${hostsFile}";

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
  system.stateVersion = "22.05"; # Did you read the comment?

}
