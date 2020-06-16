with import <stockholm/lib>;
{ config, pkgs, ... }: {

  imports = [
    <stockholm/tv>
    <stockholm/tv/2configs/hw/x220.nix>
    <stockholm/tv/2configs/exim-retiolum.nix>
    <stockholm/tv/2configs/retiolum.nix>
  ];

  boot = {
    initrd = {
      availableKernelModules = [ "ahci" ];
      luks.cryptoModules = [ "aes" "sha512" "xts" ];
      luks.devices.luksroot.device = "/dev/sda2";
    };
  };

  environment.systemPackages = with pkgs; [
    chromium
    firefoxWrapper
    networkmanagerapplet
    (pkgs.pidgin-with-plugins.override {
      plugins = [ pkgs.pidginotr ];
    })
  ];

  fileSystems = {
    "/boot" = {
      device = "/dev/sda1";
    };
    "/" = {
      device = "/dev/mapper/main-root";
      fsType = "btrfs";
      options = [ "defaults" "noatime" ];
    };
    "/home" = {
      device = "/dev/mapper/main-home";
      fsType = "btrfs";
      options = [ "defaults" "noatime" ];
    };
  };

  hardware = {
    opengl.driSupport32Bit = true;
    pulseaudio.enable = true;
  };

  i18n.defaultLocale = "de_DE.UTF-8";

  krebs.build = {
    host = config.krebs.hosts.alnus;
    user = mkForce config.krebs.users.dv;
  };

  networking.networkmanager.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
  };

  services.xserver = {
    enable = true;
    layout = "de";
    xkbOptions = "eurosign:e";

    libinput.enable = false;
    synaptics = {
      enable = true;
      twoFingerScroll = true;
    };

    desktopManager.xfce.enable = true;

    displayManager.lightdm.autoLogin.enable = true;
    displayManager.lightdm.autoLogin.user = "dv";
    displayManager.lightdm.enable = true;
  };

  users.users.dv = {
    inherit (config.krebs.users.dv) home uid;
    isNormalUser = true;
    extraGroups = [
      "audio"
      "video"
      "networkmanager"
    ];
  };
}
