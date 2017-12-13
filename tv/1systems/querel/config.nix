with import <stockholm/lib>;
{ config, pkgs, ... }: {

  imports = [
    <stockholm/krebs>
    <stockholm/tv/2configs>
    <stockholm/tv/3modules>
    <stockholm/tv/2configs/retiolum.nix>
  ];

  krebs.build.host = config.krebs.hosts.querel;
  krebs.build.user = mkForce config.krebs.users.itak;

  boot.extraModulePackages = [
    config.boot.kernelPackages.exfat-nofuse
  ];
  boot.initrd.availableKernelModules = [ "ahci" ];
  boot.initrd.luks = {
    cryptoModules = [ "aes" "sha512" "xts" ];
    devices.querel-luks1 = {
      allowDiscards = true;
      device = "/dev/sda2";
    };
  };
  boot.kernelModules = [ "kvm-intel" ];
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };

  environment.systemPackages = with pkgs; [
    firefoxWrapper
    gimp
    kate
    libreoffice
    (pkgs.pidgin-with-plugins.override {
      plugins = [ pkgs.pidginotr ];
    })
    sxiv
    texlive.combined.scheme-full
    vim
    zathura
  ];

  fileSystems = {
    "/" = {
      device = "/dev/mapper/querel-root";
      fsType = "ext4";
      options = [ "defaults" "discard" ];
    };
    "/home" = {
      device = "/dev/mapper/querel-home";
      fsType = "ext4";
      options = [ "defaults" "discard" ];
    };
    "/boot" = {
      device = "/dev/sda1";
    };
  };

  hardware.enableRedistributableFirmware = true;
  hardware.pulseaudio.enable = true;

  i18n.defaultLocale = "de_DE.UTF-8";

  networking.networkmanager.enable = true;

  programs.ssh.startAgent = false;

  services.printing = {
    enable = true;
  };

  services.xserver.enable = true;
  services.xserver.layout = "de";
  services.xserver.xkbOptions = "eurosign:e";

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
  };

  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.displayManager.auto = {
    enable = true;
    user = "itak";
  };

  users.users.itak = {
    inherit (config.krebs.users.itak) home uid;
    isNormalUser = true;
    extraGroups = [
      "audio"
      "video"
      "networkmanager"
    ];
  };
}
