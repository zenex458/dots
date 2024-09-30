# A profile with most (vanilla) hardening options enabled by default,
# potentially at the cost of stability, features and performance.
#
# This profile enables options that are known to affect system
# stability. If you experience any stability issues when using the
# profile, try disabling it. If you report an issue and use this
# profile, always mention that you do.

{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

{
  meta = {
    maintainers = [
      maintainers.joachifm
      maintainers.emily
    ];
  };

  #  security.lockKernelModules = mkDefault true;

  #  security.protectKernelImage = mkDefault true;

  boot.kernelPackages = mkDefault pkgs.linuxPackages_6_5_hardened;

  nix.settings.allowed-users = mkDefault [ "@wheel" ];

  #  security.apparmor.enable = mkDefault true;
  #  security.apparmor.packages = [ pkgs.apparmor-utils ];
  #  services.dbus.apparmor = "enabled";

  security.forcePageTableIsolation = mkDefault true;

  # This is required by podman to run containers in rootless mode.
  security.unprivilegedUsernsClone = mkDefault config.virtualisation.containers.enable;

  security.virtualisation.flushL1DataCache = mkDefault "always";

  boot.kernelParams = [
    "slab_nomerge"

    # Overwrite free'd pages
    "page_poison=1"

    # Enable page allocator randomization
    "page_alloc.shuffle=1"
    "lsm=landlock,lockdown,yama,integrity,apparmor,bpf"
    "slab_nomerge"
    "slub_debug=FZ"
    "init_on_alloc=1"
    "init_on_free=1"
    "pti=on"
    "random.trust_cpu=off"
    "spectre_v2=on"
    "spec_store_bypass_disable=on"
    "tsx=off"
    "tsx_async_abort=full"
    "kvm.nx_huge_pages=force"
    "efi=disable_early_pci_dma"
    "loglevel=3"
    "quiet"
  ];

  boot.blacklistedKernelModules = [
    # Obscure network protocols
    "ax25"
    "netrom"
    "rose"

    # Old or rare or insufficiently audited filesystems
    "adfs"
    "affs"
    "bfs"
    "befs"
    "cramfs"
    "efs"
    "erofs"
    "exofs"
    "freevxfs"
    "f2fs"
    "hfs"
    "hpfs"
    "jfs"
    "minix"
    "nilfs2"
    "omfs"
    "qnx4"
    "qnx6"
    "sysv"
    ## Disables the vivid kernel module as it's only required for testing and has been the cause of multiple vulnerabilities
    ## https://forums.whonix.org/t/kernel-recompilation-for-better-hardening/7598/233
    ## https://www.openwall.com/lists/oss-security/2019/11/02/1
    ## https://github.com/a13xp0p0v/kconfig-hardened-check/commit/981bd163fa19fccbc5ce5d4182e639d67e484475

    "vivid"
    ## Disable Intel Management Engine (ME) interface with the OS
    ## https://www.kernel.org/doc/html/latest/driver-api/mei/mei.html
    "mei"
    "mei-me"

    "bluetooth"
    "btusb"
    "msr"
    "dccp"
    "sctp"
    "rds"
    "tipc"
    "n-hdlc"
    "x25"
    "decnet"
    "econet"
    "af_802154"
    "ipx"
    "appletalk"
    "psnap"
    "p8023"
    "p8022"
    "can"
    "atm"
    "cifs"
    "nfs"
    "nfsv3"
    "nfsv4"
    "ksmbd"
    "gfs2"
    "ath_pci"
    "evbug"
    "usbmouse"
    "usbkbd"
    "eepro100"
    "de4x5"
    "eth1394"
    "snd_intel8x0m"
    "snd_aw2"
    "prism54"
    "bcm43xx"
    "garmin_gps"
    "asus_acpi"
    "snd_pcsp"
    "pcspkr"
    "amd76x_edac"
    "aty128fb"
    "atyfb"
    "radeonfb"
    "cirrusfb"
    "cyber2000fb"
    "cyblafb"
    "gx1fb"
    "hgafb"
    "i810fb"
    "intelfb"
    "kyrofb"
    "lxfb"
    "matroxfb_bases"
    "neofb"
    "nvidiafb"
    "pm2fb"
    "rivafb"
    "s1d13xxxfb"
    "savagefb"
    "sisfb"
    "sstfb"
    "tdfxfb"
    "tridentfb"
    "vesafb"
    "vfb"
    "viafb"
    "vt8623fb"
    "udlfb"
    "cdrom"
    "sr_mod"
  ];

  #prevent unprivileged attackers from loading vulnerable line disciplines
  boot.kernel.sysctl."dev.tty.ldisc_autoload" = mkDefault "0";
  #disallow opening FIFOs or regular files not owned by the user in world writable sticky directories
  boot.kernel.sysctl."fs.protected_fifos" = mkDefault "1";
  #this setting aims to mitigate kernel pointer leaks
  boot.kernel.sysctl."kernel.unprivileged_bpf_disabled" = mkDefault "1";
  #harden bpf jit for everyone
  boot.kernel.sysctl."net.core.bpf_jit_harden" = mkDefault "2";

  boot.kernel.sysctl."kernel.randomize_va_space" = mkDefault "2";
  #disables kexec which can be used to replace the running kernel.
  #  boot.kernel.sysctl."kernel.kexec_load_disabled" = mkDefault "1";
  #protects against time-wait assassination. It drops RST packets for sockets in the time-wait state.
  boot.kernel.sysctl."net.ipv4.tcp_rfc1337" = mkDefault "1";
  #These settings are set to the highest value to improve ASLR effectiveness for mmap
  boot.kernel.sysctl."vm.mmap_rnd_bits" = mkDefault "32";
  boot.kernel.sysctl."vm.mmap_rnd_compat_bits" = mkDefault "16";

  boot.kernel.sysctl."vm.swappiness" = mkOverride 60 1;
  # Restrict ptrace() usage to processes with a pre-defined relationship
  # (e.g., parent/child)
  boot.kernel.sysctl."kernel.yama.ptrace_scope" = mkOverride 500 2;

  # Hide kptrs even for processes with CAP_SYSLOG
  boot.kernel.sysctl."kernel.kptr_restrict" = mkOverride 500 1;

  # Disable bpf() JIT (to eliminate spray attacks)
  boot.kernel.sysctl."net.core.bpf_jit_enable" = mkDefault false;

  # Disable ftrace debugging
  boot.kernel.sysctl."kernel.ftrace_enabled" = mkDefault false;

  # Enable strict reverse path filtering (that is, do not attempt to route
  # packets that "obviously" do not belong to the iface's network; dropped
  # packets are logged as martians).
  boot.kernel.sysctl."net.ipv4.conf.all.log_martians" = mkDefault true;
  boot.kernel.sysctl."net.ipv4.conf.all.rp_filter" = mkDefault "1";
  boot.kernel.sysctl."net.ipv4.conf.default.log_martians" = mkDefault true;
  boot.kernel.sysctl."net.ipv4.conf.default.rp_filter" = mkDefault "1";

  # Ignore broadcast ICMP (mitigate SMURF)
  boot.kernel.sysctl."net.ipv4.icmp_echo_ignore_broadcasts" = mkDefault true;

  # Ignore incoming ICMP redirects (note: default is needed to ensure that the
  # setting is applied to interfaces added after the sysctls are set)
  boot.kernel.sysctl."net.ipv4.conf.all.accept_redirects" = mkDefault false;
  boot.kernel.sysctl."net.ipv4.conf.all.secure_redirects" = mkDefault false;
  boot.kernel.sysctl."net.ipv4.conf.default.accept_redirects" = mkDefault false;
  boot.kernel.sysctl."net.ipv4.conf.default.secure_redirects" = mkDefault false;
  boot.kernel.sysctl."net.ipv6.conf.all.accept_redirects" = mkDefault false;
  boot.kernel.sysctl."net.ipv6.conf.default.accept_redirects" = mkDefault false;

  # Ignore outgoing ICMP redirects (this is ipv4 only)
  boot.kernel.sysctl."net.ipv4.conf.all.send_redirects" = mkDefault false;
  boot.kernel.sysctl."net.ipv4.conf.default.send_redirects" = mkDefault false;

  boot.kernel.sysctl."net.ipv4.conf.all.accept_source_route" = mkDefault "0";
  boot.kernel.sysctl."net.ipv4.conf.default.accept_source_route" = mkDefault "0";
  boot.kernel.sysctl."net.ipv6.conf.all.accept_source_route" = mkDefault "0";

  # Do not accept ICMP redirects (prevent MITM attacks)
  boot.kernel.sysctl."net.ipv4.icmp_echo_ignore_all" = mkDefault "1";
  boot.kernel.sysctl."net.ipv6.icmp.echo_ignore_all" = mkDefault "1";

  # Do not accept router advertisments
  boot.kernel.sysctl."net.ipv6.conf.all.accept_ra" = mkDefault "0";
  boot.kernel.sysctl."net.ipv6.conf.default.accept_ra" = mkDefault "0";
  boot.kernel.sysctl."net.ipv4.tcp_timestamps" = mkDefault "0";

  #protect against SYN flood attacks
  boot.kernel.sysctl."net.ipv4.tcp_syncookies" = mkDefault "1";
}
