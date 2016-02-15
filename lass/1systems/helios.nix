{ config, pkgs, ... }:

with builtins;
{
  imports = [
    ../2configs/baseX.nix
    ../2configs/browsers.nix
    ../2configs/programs.nix
    ../2configs/git.nix
    ../2configs/pass.nix
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

  krebs.build.host = config.krebs.hosts.helios;

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

  #services.udev.extraRules = ''
  #  SUBSYSTEM=="net", ATTR{address}=="64:27:37:7d:d8:ae", NAME="wl0"
  #  SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:b8:c8:2e", NAME="et0"
  #'';

  services.xserver = {
    videoDriver = "intel";
    vaapiDrivers = [ pkgs.vaapiIntel ];
    deviceSection = ''
      Option "AccelMethod" "sna"
      BusID "PCI:0:2:0"
    '';
  };

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
