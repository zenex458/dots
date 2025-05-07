{
  disko.devices = {
    disk = {
      disk0 = {
        type = "disk";
<<<<<<< HEAD
        device = "/dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0";
=======
        device = "/dev/disk/by-id/usb-Kingston_DataTraveler_3.0_408D5C15CB92E911290E05C5-0:0";
>>>>>>> 465be98 (NIXOS: Updated disko config)
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
<<<<<<< HEAD
              end = "512M";
=======
              end = "256M";
>>>>>>> 465be98 (NIXOS: Updated disko config)
              #              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
              };
            };
            ESP = {
<<<<<<< HEAD
              start = "513M";
=======
              start = "257M";
>>>>>>> 465be98 (NIXOS: Updated disko config)
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
<<<<<<< HEAD
                  "--header /dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part1"
=======
                  "--header /dev/disk/by-id/usb-Kingston_DataTraveler_3.0_408D5C15CB92E911290E05C5-0:0-part1"
>>>>>>> 465be98 (NIXOS: Updated disko config)
#                  "--iter-time 1" # insecure but fast for tests
                  "--pbkdf argon2id -c serpent-xts-plain64 -h sha-512"
                ];
                extraOpenArgs = [
<<<<<<< HEAD
                  "--header /dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part1"
                ];
                settings = {
                  header = "/dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part1";
=======
                  "--header /dev/disk/by-id/usb-Kingston_DataTraveler_3.0_408D5C15CB92E911290E05C5-0:0-part1"
                ];
                settings = {
                  header = "/dev/disk/by-id/usb-Kingston_DataTraveler_3.0_408D5C15CB92E911290E05C5-0:0-part1";
>>>>>>> 465be98 (NIXOS: Updated disko config)
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
<<<<<<< HEAD
                  "--header /dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part2"
=======
                  "--header /dev/disk/by-id/usb-Kingston_DataTraveler_3.0_408D5C15CB92E911290E05C5-0:0-part2"
>>>>>>> 465be98 (NIXOS: Updated disko config)
#                  "--iter-time 1" # insecure but fast for tests
                  "--pbkdf argon2id -c serpent-xts-plain64 -h sha-512"
                ];
                extraOpenArgs = [
<<<<<<< HEAD
                  "--header /dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part2"
                ];
                settings = {
                  header = "/dev/disk/by-id/usb-SanDisk_Cruzer_Glide_04020130092220073710-0:0-part2";
=======
                  "--header /dev/disk/by-id/usb-Kingston_DataTraveler_3.0_408D5C15CB92E911290E05C5-0:0-part2"
                ];
                settings = {
                  header = "/dev/disk/by-id/usb-Kingston_DataTraveler_3.0_408D5C15CB92E911290E05C5-0:0-part2";
>>>>>>> 465be98 (NIXOS: Updated disko config)
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
