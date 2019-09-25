{ lib, pkgs, ... }:

{
  imports = [
    ./config.nix
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  boot.initrd.availableKernelModules = [ "ehci_pci" "ahci" "xhci_pci" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.loader.grub.enable = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "/dev/disk/by-id/usb-General_USB_Flash_Disk_0374116060006128-0:0";
  boot.loader.grub.efiInstallAsRemovable = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/6db29cdd-ff64-496d-b541-5f1616665dc2";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."usb_nix".device = "/dev/disk/by-uuid/3c8ab3af-57fb-4564-9e27-b2766404f5d4";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/2B9E-5131";
      fsType = "vfat";
    };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
