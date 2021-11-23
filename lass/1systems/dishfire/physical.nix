{ config, lib, pkgs, ... }:
{
  imports = [
    ./config.nix
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
  ];

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
