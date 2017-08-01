{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  krebs.build.host = config.krebs.hosts.zu;

  imports = [
    <stockholm/tv>
    <stockholm/tv/2configs/hw/x220.nix>
    <stockholm/tv/2configs/exim-retiolum.nix>
    <stockholm/tv/2configs/gitrepos.nix>
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
        gnupg1compat
        haskellPackages.hledger
        jq
        mkpasswd
        netcat
        nix-repl
        nmap
        p7zip
        pass
        q
        qrencode
        # XXX fails at systemd.services.dbus.unitConfig
        #texlive
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
      { name = "zuca"; device = "/dev/sda2"; }
    ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/mapper/zuvga-root";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/home" = {
      device = "/dev/mapper/zuvga-home";
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

  environment.systemPackages = with pkgs; [
    ethtool
    tinc_pre
    iptables
    #jack2

    gptfdisk
  ];

  security.wrappers = {
    sendmail.source = "${pkgs.exim}/bin/sendmail"; # for cron
  };

  services.printing.enable = true;

  # see tmpfiles.d(5)
  systemd.tmpfiles.rules = [
    "d /tmp 1777 root root - -" # does this work with mounted /tmp?
  ];

  #services.bitlbee.enable = true;
  #services.tor.client.enable = true;
  #services.tor.enable = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";
}
