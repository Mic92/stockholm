{
  boot.initrd.luks.devices.buda2.device = "/dev/sda2";
  fileSystems."/" = {
    device = "buda2/root";
    fsType = "zfs";
  };
  fileSystems."/home" = {
    device = "buda2/home";
    fsType = "zfs";
  };
  fileSystems."/boot" = {
    device = "/dev/sda1";
    fsType = "vfat";
  };
}
