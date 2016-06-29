{ config, lib, pkgs, ... }:

with config.krebs.lib;

{
  krebs.build.host = config.krebs.hosts.wu;

  imports = [
    ../.
    ../2configs/hw/w110er.nix
    ../2configs/exim-retiolum.nix
    ../2configs/git.nix
    ../2configs/im.nix
    ../2configs/mail-client.nix
    ../2configs/man.nix
    ../2configs/nginx/public_html.nix
    ../2configs/pulse.nix
    ../2configs/retiolum.nix
    ../2configs/wu-binary-cache
    ../2configs/xserver
    {
      environment.systemPackages = with pkgs; [

        # stockholm
        gnumake
        hashPassword
        haskellPackages.lentil
        parallel

        # root
        cryptsetup
        ntp # ntpate

        # tv
        bc
        bind # dig
        cac-api
        dic
        file
        get
        gnupg1compat
        haskellPackages.hledger
        htop
        jq
        mkpasswd
        netcat
        nix-repl
        nmap
        p7zip
        push
        qrencode
        texLive
        tmux

        #ack
        #apache-httpd
        #ascii
        #emacs
        #es
        #esniper
        #gcc
        #gptfdisk
        #graphviz
        #haskellPackages.cabal2nix
        #haskellPackages.ghc
        #haskellPackages.shake
        #hdparm
        #i7z
        #iftop
        #imagemagick
        #inotifyTools
        #iodine
        #iotop
        #lshw
        #lsof
        #minicom
        #mtools
        #ncmpc
        #neovim
        #nethogs
        #nix-prefetch-scripts #cvs bug
        #openssl
        #openswan
        #parted
        #perl
        #powertop
        #ppp
        #proot
        #pythonPackages.arandr
        #pythonPackages.youtube-dl
        #racket
        #rxvt_unicode-with-plugins
        #scrot
        #sec
        #silver-searcher
        #sloccount
        #smartmontools
        #socat
        #sshpass
        #strongswan
        #sysdig
        #sysstat
        #tcpdump
        #tlsdate
        #unetbootin
        #utillinuxCurses
        #wvdial
        #xdotool
        #xkill
        #xl2tpd
        #xsel

        unison
      ];
    }
  ];

  boot.initrd.luks = {
    cryptoModules = [ "aes" "sha512" "xts" ];
    devices = [
      { name = "home"; device = "/dev/vg840/enchome"; preLVM = false; }
    ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/mapper/vg840-wuroot";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/home" = {
      device = "/dev/mapper/home";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
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

  nixpkgs.config.chromium.enablePepperFlash = true;

  krebs.nixpkgs.allowUnfreePredicate = pkg: hasPrefix "nvidia-x11-" pkg.name;
  hardware.bumblebee.enable = true;
  hardware.bumblebee.group = "video";
  hardware.enableAllFirmware = true;
  hardware.opengl.driSupport32Bit = true;

  environment.systemPackages = with pkgs; [
    ethtool
    tinc_pre
    iptables
    #jack2
  ];

  security.setuidPrograms = [
    "sendmail"  # for cron
  ];

  services.printing.enable = true;

  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';

  # see tmpfiles.d(5)
  systemd.tmpfiles.rules = [
    "d /tmp 1777 root root - -" # does this work with mounted /tmp?
  ];

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="00:90:f5:da:aa:c3", NAME="en0"
    SUBSYSTEM=="net", ATTR{address}=="a0:88:b4:1b:ae:6c", NAME="wl0"

    # for jack
    KERNEL=="rtc0", GROUP="audio"
    KERNEL=="hpet", GROUP="audio"
  '';

  services.virtualboxHost.enable = true;
}
