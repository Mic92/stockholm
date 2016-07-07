{ config, pkgs, ... }:

with config.krebs.lib;

{
  imports = [
    ../../krebs
    ../2configs
    ../3modules
    ../2configs/exim-retiolum.nix
    ../2configs/retiolum.nix
  ];

  krebs.build.host = config.krebs.hosts.mu;
  krebs.build.user = mkForce config.krebs.users.vv;

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="00:90:f5:da:aa:c3", NAME="en0"
    SUBSYSTEM=="net", ATTR{address}=="a0:88:b4:1b:ae:6c", NAME="wl0"

    # for jack
    KERNEL=="rtc0", GROUP="audio"
    KERNEL=="hpet", GROUP="audio"
  '';


  # hardware configuration
  boot.initrd.luks.devices = [
    { name = "vgmu1"; device = "/dev/sda2"; }
  ];
  boot.initrd.luks.cryptoModules = [ "aes" "sha512" "xts" ];
  boot.initrd.availableKernelModules = [ "ahci" ];
  boot.kernelModules = [ "fbcon" "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
  '';

  fileSystems = {
    "/" = {
      device = "/dev/vgmu1/nixroot";
      fsType = "ext4";
      options = [ "defaults" "noatime" ];
    };
    "/home" = {
      device = "/dev/vgmu1/home";
      options = [ "defaults" "noatime" ];
    };
    "/boot" = {
      device = "/dev/sda1";
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "nosuid" "nodev" "noatime" ];
    };
  };

  swapDevices =[ ];

  nixpkgs.config.firefox.enableAdobeFlash = true;
  nixpkgs.config.chromium.enablePepperFlash = true;

  nixpkgs.config.allowUnfree = true;
  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;

  hardware.enableAllFirmware = true;

  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;

  environment.systemPackages = with pkgs; [
    slock
    tinc_pre
    iptables
    vim
    gimp
    xsane
    firefoxWrapper
    chromiumDev
    skype
    libreoffice
    kde4.l10n.de
    kde4.plasma-nm
    pidgin-with-plugins
    pidginotr

    kde4.print_manager
    #foomatic_filters
    #gutenprint
    #cups_pdf_filter
    #ghostscript
  ];


  i18n.defaultLocale = "de_DE.UTF-8";

  programs.ssh.startAgent = false;

  security.setuidPrograms = [
    "sendmail"  # for cron
    "slock"
  ];

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

  services.xserver.desktopManager.kde4.enable = true;
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

  # see tmpfiles.d(5)
  systemd.tmpfiles.rules = [
    "d /tmp 1777 root root - -" # does this work with mounted /tmp?
  ];
}
