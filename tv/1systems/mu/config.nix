with import <stockholm/lib>;
{ config, pkgs, ... }: {

  imports = [
    <stockholm/krebs>
    <stockholm/tv/2configs>
    <stockholm/tv/3modules>
    <stockholm/tv/2configs/exim-retiolum.nix>
    <stockholm/tv/2configs/retiolum.nix>
  ];

  krebs.build.host = config.krebs.hosts.mu;
  krebs.build.user = mkForce config.krebs.users.vv;

  tv.x0vncserver.enable = true;

  # hardware configuration
  boot.initrd.luks.devices.muca = {
    device = "/dev/disk/by-uuid/a8796bb3-6c03-4ddf-b2e4-c2e44c51d352";
  };
  boot.initrd.luks.cryptoModules = [ "aes" "sha512" "xts" ];
  boot.initrd.availableKernelModules = [ "ahci" ];
  boot.kernelModules = [ "fbcon" "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems = {
    "/" = {
      device = "/dev/mapper/muvga-root";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/home" = {
      device = "/dev/mapper/muvga-home";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/DC38-F165";
    };
  };

  nixpkgs.config.allowUnfree = true;
  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;

  hardware.enableRedistributableFirmware = true;

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

  networking.networkmanager.enable = true;

  environment.systemPackages = with pkgs; [
    slock
    tinc_pre
    iptables
    vim
    gimp
    xsane
    firefoxWrapper
    chromium
    skype
    libreoffice
    pidgin-with-plugins
    pidginotr

    #foomatic_filters
    #gutenprint
    #cups_pdf_filter
    #ghostscript
  ];


  i18n.defaultLocale = "de_DE.UTF-8";

  programs.ssh.startAgent = false;

  security.wrappers = {
    slock.source = "${pkgs.slock}/bin/slock";
  };

  security.pam.loginLimits = [
    # for jack
    { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
    { domain = "@audio"; item = "rtprio"; type = "-"; value = "99"; }
  ];

  fonts.fonts = [
    pkgs.xlibs.fontschumachermisc
  ];

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    #drivers = [
    #  #pkgs.foomatic_filters
    #  #pkgs.gutenprint
    #  #pkgs.cups_pdf_filter
    #  #pkgs.ghostscript
    #];
    #cupsdConf = ''
    #  LogLevel debug2
    #'';
  };

  services.xserver.enable = true;
  services.xserver.layout = "de";
  services.xserver.xkbOptions = "eurosign:e";

  # TODO this is host specific
  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
  };

  services.xserver.desktopManager.plasma5 = {
    enable = true;
  };
  services.xserver.displayManager.auto = {
    enable = true;
    user = "vv";
  };

  users.users.vv = {
    inherit (config.krebs.users.vv) home uid;
    isNormalUser = true;
    extraGroups = [
      "audio"
      "video"
      "networkmanager"
    ];
  };
}
