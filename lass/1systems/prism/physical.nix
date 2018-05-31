{ config, lib, pkgs, ... }:
{
  imports = [
    ./config.nix
    {
      networking.interfaces.et0.ipv4.addresses = [
        {
          address = config.krebs.build.host.nets.internet.ip4.addr;
          prefixLength = 27;
        }
        {
          address = "46.4.114.243";
          prefixLength = 27;
        }
      ];
      networking.defaultGateway = "46.4.114.225";
      networking.nameservers = [
        "8.8.8.8"
      ];
      services.udev.extraRules = ''
        SUBSYSTEM=="net", ATTR{address}=="08:60:6e:e7:87:04", NAME="et0"
      '';
    }
    {
      imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

      boot.loader.grub = {
        devices = [
          "/dev/sda"
          "/dev/sdb"
        ];
        splashImage = null;
      };

      boot.initrd.availableKernelModules = [
        "ata_piix"
        "vmw_pvscsi"
        "ahci" "sd_mod"
      ];

      boot.kernelModules = [ "kvm-intel" ];

      fileSystems."/" = {
        device = "/dev/pool/nix_root";
        fsType = "ext4";
      };

      fileSystems."/tmp" = {
        device = "tmpfs";
        fsType = "tmpfs";
        options = ["nosuid" "nodev" "noatime"];
      };

      fileSystems."/var/download" = {
        device = "/dev/pool/download";
        fsType = "ext4";
      };

      fileSystems."/srv/http" = {
        device = "/dev/pool/http";
        fsType = "ext4";
      };

      fileSystems."/home" = {
        device = "/dev/pool/home";
        fsType = "ext4";
      };

      fileSystems."/bku" = {
        device = "/dev/pool/bku";
        fsType = "ext4";
      };

      swapDevices = [
        { label = "swap1"; }
        { label = "swap2"; }
      ];

      sound.enable = false;
      nixpkgs.config.allowUnfree = true;
      time.timeZone = "Europe/Berlin";
    }
  ];

}
