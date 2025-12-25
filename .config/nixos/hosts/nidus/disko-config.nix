let
  # the number of headers to disks is a to one to one map
  headerdevice = "/dev/disk/by-id/usb-Kingston_DataTraveler_3.0_408D5C15CB92E911290E05C5-0:0";
  header1 = headerdevice + "-part1"; #luks header for nvme
in {
  disko.devices = {
    disk = {
      disk0 = {
        type = "disk";
        device = headerdevice;
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
            ESP = {
              start = "129M";
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
        device = "/dev/disk/by-id/nvme-SAMSUNG_MZVLB512HBJQ-000L7_S4ENNX2NC35223";
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
                  "--header ${header1}"
                  #                  "--iter-time 1" # insecure but fast for tests
                  "--pbkdf argon2id -c serpent-xts-plain64 -h blake2b-512 --iter-time 5000"
                ];
                extraOpenArgs = [
                  "--header ${header1}"
                ];
                settings = {
                  header = header1;
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
          swap = {
            size = "32G";
            content = {
              type = "swap";
            };
          };
          root = {
            size = "100%";
            content = {
              type = "btrfs";
              extraArgs = ["-f"];
              subvolumes = {
                "/root" = {
                  mountpoint = "/";
                  mountOptions = ["compress=zstd" "noatime"];
                  # mountOptions = ["noatime"];
                };
                "/persistent" = {
                  mountOptions = ["subvol=persistent" "compress=zstd" "noatime"];
                  # mountOptions = ["subvol=persistent" "noatime"];
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
                  mountOptions = ["compress=zstd" "noatime"];
                  # mountOptions = ["noatime"];
                  mountpoint = "/tmp";
                };
              };
            };
          };
        };
      };
    };
  };
}
