_:

{
  imports = [ <nixpkgs/nixos/modules/profiles/qemu-guest.nix> ];

  boot.loader.grub = {
    device = "/dev/sda";
    splashImage = null;
  };
  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "sd_mod" "sr_mod" ];

  fileSystems."/" = {
    device = "/dev/sda1";
    fsType = "ext4";
  };
}
