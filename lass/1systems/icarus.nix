{ config, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    ../.
    ../2configs/retiolum.nix
    ../2configs/hw/tp-x220.nix
    ../2configs/baseX.nix
    ../2configs/git.nix
    ../2configs/exim-retiolum.nix
    ../2configs/browsers.nix
    ../2configs/programs.nix
    ../2configs/fetchWallpaper.nix
    ../2configs/backups.nix
    ../2configs/games.nix
    #{
    #  users.extraUsers = {
    #    root = {
    #      openssh.authorizedKeys.keys = map readFile [
    #        ../../krebs/Zpubkeys/uriel.ssh.pub
    #      ];
    #    };
    #  };
    #}
  ];

  krebs.build.host = config.krebs.hosts.icarus;

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
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="08:11:96:0a:5d:6c", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:71:cb:35", NAME="et0"
  '';
}
