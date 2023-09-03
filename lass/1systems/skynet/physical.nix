{
  imports = [
    ./config.nix
    <stockholm/krebs/2configs/hw/x220.nix>
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;
  boot.loader.grub.device = "nodev";

  networking.hostId = "06442b9a";

  fileSystems."/" = {
    device = "rpool/root";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/0876-B308";
    fsType = "vfat";
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="10:0b:a9:a6:44:04", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:d1:90:fc", NAME="et0"
  '';
}
