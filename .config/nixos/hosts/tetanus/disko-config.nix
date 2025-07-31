let
  # the number of headers to disks is a to one to one map
  headerdevice = "/dev/disk/by-id/usb-Kingston_DataTraveler_3.0_408D5C15CB92E911290E05C5-0:0";
  header1 = headerdevice + "-part1"; #luks header for nvme
  header2 = headerdevice + "-part2"; #luks header for hdd
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
            HED2 = {
              start = "129M";
              end = "256M";
              #              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
              };
            };
            ESP = {
              start = "257M";
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
                  "--header ${header2}"
                  #                  "--iter-time 1" # insecure but fast for tests
                  "--pbkdf argon2id -c serpent-xts-plain64 -h blake2b-512 --iter-time 5000"
                ];
                extraOpenArgs = [
                  "--header ${header2}"
                ];
                settings = {
                  header = header2;
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
            size = "12G";
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
                  #mountOptions = ["compress=zstd" "noatime"];
                  mountOptions = ["noatime"];
                };
                "/persistent" = {
                  #mountOptions = ["subvol=persistent" "compress=zstd" "noatime"];
                  mountOptions = ["subvol=persistent" "noatime"];
                  mountpoint = "/persistent";
                };
                "/nix" = {
                  mountOptions = [
                    #"compress=zstd"
                    "noatime"
                    "subvol=nix"
                  ];
                  mountpoint = "/nix";
                };
                "/tmp" = {
                  #mountOptions = ["compress=zstd" "noatime"];
                  mountOptions = ["noatime"];
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
