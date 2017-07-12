{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  krebs.build.host = config.krebs.hosts.wu;

  imports = [
    <stockholm/tv>
    <stockholm/tv/2configs/hw/w110er.nix>
    <stockholm/tv/2configs/exim-retiolum.nix>
    <stockholm/tv/2configs/gitrepos.nix>
    <stockholm/tv/2configs/im.nix>
    <stockholm/tv/2configs/mail-client.nix>
    <stockholm/tv/2configs/man.nix>
    <stockholm/tv/2configs/nginx/public_html.nix>
    <stockholm/tv/2configs/pulse.nix>
    <stockholm/tv/2configs/retiolum.nix>
    <stockholm/tv/2configs/xserver>
    {
      environment.systemPackages = with pkgs; [
        # root
        cryptsetup

        # tv
        bc
        bind # dig
        cac-api
        dic
        file
        get
        gnupg1compat
        haskellPackages.hledger
        jq
        mkpasswd
        netcat
        nix-repl
        nmap
        p7zip
        push
        qrencode
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
      { name = "wuca"; device = "/dev/sda2"; }
    ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/mapper/wuvga-root";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/bku" = {
      device = "/dev/mapper/wuvga-bku";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/home" = {
      device = "/dev/mapper/wuvga-home";
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

  krebs.nixpkgs.allowUnfreePredicate = pkg: hasPrefix "nvidia-x11-" pkg.name;
  hardware.bumblebee.enable = true;
  hardware.bumblebee.group = "video";
  hardware.enableRedistributableFirmware= true;
  hardware.opengl.driSupport32Bit = true;

  environment.systemPackages = with pkgs; [
    ethtool
    tinc_pre
    iptables
    #jack2
  ];

  security.wrappers = {
    sendmail.source = "${pkgs.exim}/bin/sendmail"; # for cron
  };

  services.printing.enable = true;

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

  virtualisation.virtualbox.host.enable = true;
}
