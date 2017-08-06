{ config, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>
    #TODO reinstall with correct layout and use lass/hw/x220
    <stockholm/krebs/2configs/hw/x220.nix>

    <stockholm/lass/2configs/mouse.nix>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/git.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/backups.nix>
    <stockholm/lass/2configs/wine.nix>
    <stockholm/lass/2configs/bitcoin.nix>
  ];

  krebs.build.host = config.krebs.hosts.shodan;

  boot = {
    loader.grub.enable = true;
    loader.grub.version = 2;
    loader.grub.device = "/dev/sda";

    initrd.luks.devices = [ { name = "luksroot"; device = "/dev/sda2"; } ];
    initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
    initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];
    #kernelModules = [ "kvm-intel" "msr" ];
  };
  fileSystems = {
    "/" = {
      device = "/dev/pool/nix";
      fsType = "btrfs";
    };

    "/boot" = {
      device = "/dev/sda1";
    };

    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = ["nosuid" "nodev" "noatime"];
    };
    "/bku" = {
      device = "/dev/pool/bku";
      fsType = "btrfs";
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="a0:88:b4:29:26:bc", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:0c:a7:63", NAME="et0"
  '';
}
