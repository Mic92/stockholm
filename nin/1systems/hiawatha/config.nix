# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    <stockholm/nin>
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    #../2configs/copyq.nix
    <stockholm/nin/2configs/games.nix>
    <stockholm/nin/2configs/git.nix>
    <stockholm/nin/2configs/retiolum.nix>
    <stockholm/nin/2configs/termite.nix>
  ];

  krebs.build.host = config.krebs.hosts.hiawatha;

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "sd_mod" "sr_mod" "rtsx_pci_sdmmc" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/b83f8830-84f3-4282-b10e-015c4b76bd9e";
      fsType = "ext4";
    };

  fileSystems."/tmp" =
    { device = "tmpfs";
      fsType = "tmpfs";
    };

  fileSystems."/home" =
    { device = "/dev/fam/home";
    };


  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/2f319b08-2560-401d-b53c-2abd28f1a010";
      fsType = "ext2";
    };

  boot.initrd.luks.devices = [ { name = "luksroot"; device = "/dev/sda2"; } ];
  boot.initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 4;
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  fileSystems."/home/nin/.local/share/Steam" = {
    device = "/dev/fam/steam";
  };

  # nin config
  time.timeZone = "Europe/Berlin";
  services.xserver.enable = true;

  networking.networkmanager.enable = true;
  #networking.wireless.enable = true;

  hardware.pulseaudio = {
    enable = true;
    systemWide = true;
  };

  hardware.bluetooth.enable = true;

  hardware.opengl.driSupport32Bit = true;

  #nixpkgs.config.steam.java = true;

  environment.systemPackages = with pkgs; [
    firefox
    git
    networkmanagerapplet
    python
    steam
    thunderbird
    vim
    virtmanager
  ];

  nixpkgs.config = {

    allowUnfree = true;

    firefox = {
      enableGoogleTalkPlugin = true;
      enableAdobeFlash = true;
    };
  };

  #services.logind.extraConfig = "HandleLidSwitch=ignore";

  services.xserver.synaptics = {
    enable = true;
  };


  services.xserver.desktopManager.xfce = let
    xbindConfig = pkgs.writeText "xbindkeysrc" ''
      "${pkgs.pass}/bin/passmenu --type"
        Control + p
  '';
  in {
    enable = true;
      extraSessionCommands = ''
      ${pkgs.xbindkeys}/bin/xbindkeys -f ${xbindConfig}
    '';
  };

 # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

}
