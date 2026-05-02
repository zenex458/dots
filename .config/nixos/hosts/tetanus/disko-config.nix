{
  disko.devices = {
    disk = {
      disk0 = {
        type = "disk";
        device = "/dev/disk/by-id/nvme-UMIS_RPJTJ128MEE1MWX_SS1B60642Z1CD26A26BY";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "1G";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
            luks = {
              size = "100%";
              content = {
                type = "luks";
                name = "crypted1";
                content = {
                  type = "lvm_pv";
                  vg = "pool";
                };
              };
            };
          };
        };
      };
      disk1 = {
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
            size = "8G";
            content = {
              type = "swap";
              mountOptions = [ "discard" ];
            };
          };
          root = {
            size = "100%";
            content = {
              type = "btrfs";
              extraArgs = [ "-f" ];
              subvolumes = {
                "/root" = {
                  mountpoint = "/";
                  #mountOptions = ["compress=zstd" "noatime"];
                  mountOptions = [ "noatime" ];
                };
                "/persistent" = {
                  #mountOptions = ["subvol=persistent" "compress=zstd" "noatime"];
                  mountOptions = [
                    "subvol=persistent"
                    "noatime"
                  ];
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
                  mountOptions = [ "noatime" ];
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
