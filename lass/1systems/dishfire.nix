{ config, lib, pkgs, ... }:

{
  imports = [
    ../.
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    ../2configs/base.nix
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
  ];

  krebs.build.host = config.krebs.hosts.dishfire;
}
