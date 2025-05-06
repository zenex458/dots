{
  disko.devices = {
    disk = {
      disk0 = {
        type = "disk";
        device = "/dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0";
        content = {
          type = "gpt";
          partitions = {
            HED1 = {
              start = "1M";
              end = "128M";
              #              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
              };
            };
            HED2 = {
              start = "129M";
              end = "512M";
              #              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
              };
            };
            ESP = {
              start = "513M";
              size = "100%";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = ["umask=0077"];
              };
            };
          };
        };
      };
      disk1 = {
        type = "disk";
        device = "/dev/disk/by-id/nvme-UMIS_RPJTJ128MEE1MWX_SS1B60642Z1CD26A26BY";
        content = {
          type = "gpt";
          partitions = {
            luks = {
              start = "1M";
              size = "100%";
              content = {
                type = "luks";
                name = "crypted1";
                extraFormatArgs = [
                  "--header /dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part1"
#                  "--iter-time 1" # insecure but fast for tests
                  "--pbkdf argon2id -c serpent-xts-plain64 -h sha-512"
                ];
                extraOpenArgs = [
                  "--header /dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part1"
                ];
                settings = {
                  header = "/dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part1";
                };
                content = {
                  type = "lvm_pv";
                  vg = "pool";
                };
              };
            };
          };
        };
      };
      disk2 = {
        type = "disk";
        device = "/dev/disk/by-id/ata-KINGSTON_SA400S37240G_50026B7785918FC7";
        content = {
          type = "gpt";
          partitions = {
            luks = {
              start = "1M";
              size = "100%";
              content = {
                type = "luks";
                name = "crypted2";
                extraFormatArgs = [
                  "--header /dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part2"
#                  "--iter-time 1" # insecure but fast for tests
                  "--pbkdf argon2id -c serpent-xts-plain64 -h sha-512"
                ];
                extraOpenArgs = [
                  "--header /dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part2"
                ];
                settings = {
                  header = "/dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part2";
                };
                content = {
                  type = "lvm_pv";
                  vg = "pool";
                };
              };
            };
          };
        };
      };
    };

    lvm_vg = {
      pool = {
        type = "lvm_vg";
        lvs = {
          root = {
            size = "100%";
            content = {
              type = "btrfs";
              #              format = "btrfs";
              extraArgs = ["-f"];
              subvolumes = {
                "/root" = {
                  mountpoint = "/";
                };
                "/persistent" = {
                  mountOptions = ["subvol=persistent" "compress=zstd"];
                  mountpoint = "/persistent";
                };
                "/nix" = {
                  mountOptions = [
                    "compress=zstd"
                    "noatime"
                    "subvol=nix"
                  ];
                  mountpoint = "/nix";
                };
                "/tmp" = {
                  # mountOptions = ["compress=zstd" "noatime" "noexec"];
                  mountOptions = ["compress=zstd" "noatime"];
                  mountpoint = "/tmp";
                };
                "/swap" = {
                  mountpoint = "/.swapvol";
                  swap.swapfile.size = "12G";
                };
              };
            };
          };
        };
      };
    };
  };
}
