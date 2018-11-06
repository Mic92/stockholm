{ config, lib, pkgs, ... }:
{
  imports = [
    ./config.nix
    {
      boot.kernelParams = [ "net.ifnames=0" ];
      networking = {
        defaultGateway = "46.4.114.225";
        # Use google's public DNS server
        nameservers = [ "8.8.8.8" ];
        interfaces.eth0 = {
          ipAddress = "46.4.114.247";
          prefixLength = 27;
        };
      };
      # TODO use this network config
      #networking.interfaces.et0.ipv4.addresses = [
      #  {
      #    address = config.krebs.build.host.nets.internet.ip4.addr;
      #    prefixLength = 27;
      #  }
      #  {
      #    address = "46.4.114.243";
      #    prefixLength = 27;
      #  }
      #];
      #networking.defaultGateway = "46.4.114.225";
      #networking.nameservers = [
      #  "8.8.8.8"
      #];
      #services.udev.extraRules = ''
      #  SUBSYSTEM=="net", ATTR{address}=="08:60:6e:e7:87:04", NAME="et0"
      #'';
    }
    {
      imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

      networking.hostId = "fb4173ea";
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

      sound.enable = false;
      nixpkgs.config.allowUnfree = true;
      time.timeZone = "Europe/Berlin";

      fileSystems."/" = {
        device = "rpool/root/nixos";
        fsType = "zfs";
      };

      fileSystems."/home" = {
        device = "rpool/home";
        fsType = "zfs";
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/b67c3370-1597-4ce8-8a46-e257ca32150d";
        fsType = "ext4";
      };

    }
  ];

}
