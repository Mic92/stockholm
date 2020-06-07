{
  imports = [
    ./config.nix
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;
  boot.loader.grub.device = "nodev";

  networking.hostId = "06442b9a";

  fileSystems."/" = {
    device = "/dev/pool/root";
    fsType = "btrfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/1F60-17C6";
    fsType = "vfat";
  };

  fileSystems."/home" = {
    device = "/dev/pool/home";
    fsType = "btrfs";
  };

  fileSystems."/tmp" = {
    device = "tmpfs";
    fsType = "tmpfs";
    options = ["nosuid" "nodev" "noatime"];
  };
  boot.initrd.luks = {
    cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
    devices =  [{
       name = "luksroot";
       device = "/dev/nvme0n1p3";
    }];
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="f8:59:71:a9:05:65", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="54:e1:ad:4f:06:83", NAME="et0"
  '';
}
