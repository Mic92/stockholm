{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  krebs.build.host = config.krebs.hosts.xu;

  imports = [
    ../.
    ../2configs/hw/x220.nix
    ../2configs/exim-retiolum.nix
    ../2configs/gitconfig.nix
    ../2configs/gitrepos.nix
    ../2configs/mail-client.nix
    ../2configs/man.nix
    ../2configs/nginx/public_html.nix
    ../2configs/pulse.nix
    ../2configs/retiolum.nix
    ../2configs/binary-cache
    ../2configs/xserver
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
        htop
        jq
        mkpasswd
        netcat
        netcup
        nix-repl
        nmap
        p7zip
        pass
        q
        qrencode
        texlive.combined.scheme-full
        tmux

        (pkgs.writeDashBin "krebszones" ''
          set -efu
          export OVH_ZONE_CONFIG=$HOME/.secrets/krebs/ovh-zone.conf
          case $* in
            import)
              set -- import /etc/zones/krebsco.de krebsco.de
              echo "+ krebszones $*" >&2
              ;;
          esac
          exec ${pkgs.krebszones}/bin/ovh-zone "$@"
        '')

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
      { name = "xuca"; device = "/dev/sda2"; }
    ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/mapper/xuvga-root";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/bku" = {
      device = "/dev/mapper/xuvga-bku";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/home" = {
      device = "/dev/mapper/xuvga-home";
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
  #services.virtualboxHost.enable = true;


  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";
}
