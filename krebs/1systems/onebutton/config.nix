{ config, pkgs, lib, ... }:
{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    { # minimal disk usage
      environment.noXlibs = true;
      nix.gc.automatic = true;
      nix.gc.dates = "03:10";
      programs.info.enable = false;
      programs.man.enable = false;
      services.journald.extraConfig = "SystemMaxUse=50M";
      services.nixosManual.enable = false;
    }
  ];
  krebs.build.host = config.krebs.hosts.onebutton;
  # NixOS wants to enable GRUB by default
  boot.loader.grub.enable = false;
  # Enables the generation of /boot/extlinux/extlinux.conf
  boot.loader.generic-extlinux-compatible.enable = true;

  # !!! If your board is a Raspberry Pi 1, select this:
  boot.kernelPackages = pkgs.linuxPackages_rpi;

  nix.binaryCaches = [ "http://nixos-arm.dezgeg.me/channel" ];
  nix.binaryCachePublicKeys = [ "nixos-arm.dezgeg.me-1:xBaUKS3n17BZPKeyxL4JfbTqECsT+ysbDJz29kLFRW0=%" ];

  # !!! Needed for the virtual console to work on the RPi 3, as the default of 16M doesn't seem to be enough.
  # boot.kernelParams = ["cma=32M"];

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-label/NIXOS_BOOT";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };

  swapDevices = [ { device = "/swapfile"; size = 1024; } ];
  services.openssh.enable = true;
}
