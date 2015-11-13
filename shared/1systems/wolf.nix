{ config, lib, pkgs, ... }:

{
  imports = [
    ../2configs/base.nix
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    ../2configs/collectd-base.nix
  ];

  krebs.build.host = config.krebs.hosts.wolf;
  # TODO rename shared user to "krebs"
  krebs.build.user = config.krebs.users.shared;
  krebs.build.target = "wolf";

  boot.kernel.sysctl = {
    # Enable IPv6 Privacy Extensions
    "net.ipv6.conf.all.use_tempaddr" = 2;
    "net.ipv6.conf.default.use_tempaddr" = 2;
  };

  boot.initrd.availableKernelModules = [
    "ata_piix" "uhci_hcd" "ehci_pci" "virtio_pci" "virtio_blk"
  ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";

  fileSystems."/" = { device = "/dev/disk/by-label/nixos"; fsType = "ext4"; };

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];

  time.timeZone = "Europe/Berlin";
}
