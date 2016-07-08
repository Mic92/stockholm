{ config, pkgs, ... }:

with builtins;
with config.krebs.lib;

{
  imports = [
    ../.
    ../2configs/exim-retiolum.nix
    ../2configs/browsers.nix
    ../2configs/programs.nix
    ../2configs/git.nix
    ../2configs/pass.nix
    ../2configs/fetchWallpaper.nix
    ../2configs/backups.nix

    #{
    #  # conflicting stuff with gnome setup
    #  # TODO: fix this
    #  imports = [
    #    ../2configs/baseX.nix
    #  ];
    #  networking.wireless.enable = true;
    #}
    {
      # gnome3 for suja
      imports = [
        ../2configs/default.nix
      ];
      services.xserver.enable = true;
      services.xserver.desktopManager.gnome3.enable = true;
      users.users.suja = {
        uid = genid "suja";
        home = "/home/suja";
        group = "users";
        createHome = true;
        useDefaultShell = true;
        extraGroups = [
        ];
      };
      environment.systemPackages = with pkgs; [
        firefox
        chromium
      ];
    }
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
      services.elasticsearch = {
        enable = true;
      };
    }
    {
      lass.power-action.battery = "BAT1";
    }
  ];

  krebs.build.host = config.krebs.hosts.helios;


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

    "/home" = {
      device = "/dev/pool/home";
      fsType = "ext4";
    };

    "/bku" = {
      device = "/dev/pool/bku";
      fsType = "ext4";
    };
  };

  #services.udev.extraRules = ''
  #  SUBSYSTEM=="net", ATTR{address}=="64:27:37:7d:d8:ae", NAME="wl0"
  #  SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:b8:c8:2e", NAME="et0"
  #'';

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    accelFactor = "0.035";
    additionalOptions = ''
      Option "FingerHigh" "60"
      Option "FingerLow"  "60"
    '';
  };
}
