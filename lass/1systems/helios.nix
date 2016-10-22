{ config, pkgs, ... }:

with builtins;
with import <stockholm/lib>;

{
  imports = [
    ../.
    ../2configs/retiolum.nix
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
    #}
    {
      # gnome3 for suja
      time.timeZone = "Europe/Berlin";
      services.xserver.enable = true;
      services.xserver.desktopManager.xfce.enable = true;
      networking.wireless.enable = true;
      users.users.ferret = {
        uid = genid "ferret";
        home = "/home/ferret";
        group = "users";
        createHome = true;
        useDefaultShell = true;
        extraGroups = [
        ];
        hashedPassword = "$6$SaneLuyep90p8BPn$0IDbvLgNbRGZL96obWavanTmY6IkBG84vs2b/2oqlpbmTZH3retOYbQKF1uVqu6dD0ZGF4eBq9tqPbwUjRyY00";
      };
      environment.systemPackages = with pkgs; [
        firefox
        chromium
        jre
        maven
        arandr
        libreoffice
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
    #{
    #  services.elasticsearch = {
    #    enable = true;
    #  };
    #}
    {
      krebs.power-action.battery = "BAT1";
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
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = ["nosuid" "nodev" "noatime"];
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
