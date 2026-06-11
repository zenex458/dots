{ inputs, utils, ... }:
{
  imports = [ inputs.impermanence.nixosModules.impermanence ];

  fileSystems."/persistent".neededForBoot = true;
  environment.persistence."/persistent" = {
    enable = true; # NB: Defaults to true, not needed
    hideMounts = true;
    directories = [
      "/etc/nixos"
      "/var/log"
      "/var/tmp"
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
        directory = "/var/lib/netbird";
        user = "root";
        group = "root";
        mode = "0700";
      }
      {
        directory = "/var/keys";
        user = "root";
        group = "root";
        mode = "0700";
      }
    ];

    files = [ "/etc/machine-id" ];

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

  systemd.tmpfiles.rules = [
    "d /persistent/var/keys/ 0700 root root"
    "d /persistent/etc/nixos/ 0700 root root"
  ];

  users = {
    mutableUsers = false;
    users = {
      root.hashedPasswordFile = "/persistent/var/keys/rootP";
      zenex.hashedPasswordFile = "/persistent/var/keys/zenexP";
    };
  };

  #https://github.com/nix-community/impermanence/pull/321
  boot.initrd.systemd = {
    enable = true; # Default in 26.05
    services.wipe-file-systems = {
      # Specify dependencies explicitly
      unitConfig.DefaultDependencies = false;
      # The script needs to run to completion before this service is done
      serviceConfig.Type = "oneshot";
      # This service is required for boot to succeed
      requiredBy = [ "initrd.target" ];
      # Should complete before any file systems are mounted
      before = [ "sysroot.mount" ];

      # Wait for the disk to appear
      requires = [ "${utils.escapeSystemdPath "/dev/pool/root"}.device" ];
      after = [
        "${utils.escapeSystemdPath "/dev/pool/root"}.device"
        # Allow hibernation to resume before trying to alter any data
        "local-fs-pre.target"
      ];
      script = ''
        mkdir /btrfs_tmp
        mount /dev/pool/root /btrfs_tmp
        if [[ -e /btrfs_tmp/root ]]; then
            mkdir -p /btrfs_tmp/old_roots
            timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
            mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
        fi
        for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +15); do
            btrfs subvolume delete --recursive "$i"
        done

        btrfs subvolume create /btrfs_tmp/root
        umount /btrfs_tmp
      '';
    };
  };
}
