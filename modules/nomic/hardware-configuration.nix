{ config, ... }:

{
  boot.initrd.luks = {
    cryptoModules = [ "aes" "sha1" "xts" ];
    devices = [
      {
        name = "luks1";
        device = "/dev/disk/by-uuid/cac73902-1023-4906-8e95-3a8b245337d4";
      }
    ];
  };

  boot.initrd.availableKernelModules = [ "ahci" ];
  boot.kernelModules = [ "kvm-intel" "wl" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];

  boot.loader.grub = {
    device = "/dev/sda";
    splashImage = null;
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/de4780fc-0473-4708-81df-299b7383274c";
      fsType = "btrfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/be3a1d80-3157-4d7c-86cc-ef01b64eff5e";
      fsType = "ext4";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/9db9c8ff-51da-4cbd-9f0a-0cd3333bbaff";
      fsType = "btrfs";
    };

  swapDevices = [ ];

  nix = {
    buildCores = 2;
    maxJobs = 2;
    daemonIONiceLevel = 1;
    daemonNiceLevel = 1;
  };

  # For config.boot.kernelPackages.broadcom_sta
  nixpkgs.config.allowUnfree = true;
}
