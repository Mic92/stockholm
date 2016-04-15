{ config, lib, pkgs, ... }:

{
  imports = [
    ../.
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    ../2configs/default.nix
    ../2configs/exim-retiolum.nix
    ../2configs/git.nix
    ../2configs/websites/fritz.nix
    {
      boot.loader.grub = {
        device = "/dev/vda";
        splashImage = null;
      };

      boot.initrd.availableKernelModules = [
        "ata_piix"
        "ehci_pci"
        "uhci_hcd"
        "virtio_pci"
        "virtio_blk"
      ];

      fileSystems."/" = {
        device = "/dev/mapper/pool-nix";
        fsType = "ext4";
      };

      fileSystems."/srv/http" = {
        device = "/dev/pool/srv_http";
        fsType = "ext4";
      };

      fileSystems."/boot" = {
        device = "/dev/vda1";
        fsType = "ext4";
      };
      fileSystems."/bku" = {
        device = "/dev/pool/bku";
        fsType = "ext4";
      };
    }
    {
      networking.dhcpcd.allowInterfaces = [
        "enp*"
        "eth*"
      ];
    }
    {
      sound.enable = false;
    }
    {
      environment.systemPackages = with pkgs; [
        mk_sql_pair
      ];
    }
  ];

  krebs.build.host = config.krebs.hosts.dishfire;
}
