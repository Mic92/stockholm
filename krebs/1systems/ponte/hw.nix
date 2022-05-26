{ modulesPath, ... }:
{
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];
  boot.loader.efi.efiSysMountPoint = "/boot/EFI";
  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
    device = "nodev";
    copyKernels = false;
  };
  boot.initrd.kernelModules = [ "nvme" ];
  fileSystems."/" = { device = "/dev/sda1"; fsType = "ext4"; };
  fileSystems."/boot/EFI" = { device = "/dev/disk/by-uuid/628A-7F3B"; fsType = "vfat"; };
}
