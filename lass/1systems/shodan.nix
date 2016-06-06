{ config, pkgs, ... }:

with builtins;
{
  imports = [
    ../.
    ../2configs/baseX.nix
    ../2configs/exim-retiolum.nix
    ../2configs/browsers.nix
    ../2configs/programs.nix
    ../2configs/fetchWallpaper.nix
    ../2configs/backups.nix
    #{
    #  users.extraUsers = {
    #    root = {
    #      openssh.authorizedKeys.keys = map readFile [
    #        ../../krebs/Zpubkeys/uriel.ssh.pub
    #      ];
    #    };
    #  };
    #}
    {
      #x220 config from mors
      #TODO: make x220 config file (or look in other user dir)
      hardware.trackpoint = {
        enable = true;
        sensitivity = 220;
        speed = 0;
        emulateWheel = true;
      };

      services.xserver = {
        videoDriver = "intel";
        vaapiDrivers = [ pkgs.vaapiIntel ];
        deviceSection = ''
          Option "AccelMethod" "sna"
          BusID "PCI:0:2:0"
        '';
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.shodan;

  networking.wireless.enable = true;

  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;

  boot = {
    loader.grub.enable = true;
    loader.grub.version = 2;
    loader.grub.device = "/dev/sda";

    initrd.luks.devices = [ { name = "luksroot"; device = "/dev/sda2"; } ];
    initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
    initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];
    #kernelModules = [ "kvm-intel" "msr" ];
    kernelModules = [ "msr" ];
  };
  fileSystems = {
    "/" = {
      device = "/dev/pool/nix";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/sda1";
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="a0:88:b4:29:26:bc", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:0c:a7:63", NAME="et0"
  '';
}
