{ ... }:
{
  imports = [
    <stockholm/krebs/2configs/hw/x220.nix>
  ];

  boot = {
    initrd.luks.devices = [ { name = "luksroot"; device = "/dev/sda3"; } ];
    initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
    initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];
  };
  fileSystems = {
    "/" = {
      device = "/dev/mapper/pool-root";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/boot" = {
      device = "/dev/sda2";
    };
    "/home" = {
      device = "/dev/mapper/pool-home";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = ["nosuid" "nodev" "noatime"];
    };
  };
}
