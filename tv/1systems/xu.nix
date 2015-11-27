{ config, lib, pkgs, ... }:

with lib;

{
  krebs.build.host = config.krebs.hosts.xu;

  krebs.build.source.git.nixpkgs.rev =
    "7ae05edcdd14f6ace83ead9bf0d114e97c89a83a";

  imports = [
    ../2configs/hw/x220.nix
    #../2configs/consul-client.nix
    ../2configs/git.nix
    ../2configs/mail-client.nix
    ../2configs/pulse.nix
    ../2configs/xserver
    {
      environment.systemPackages = with pkgs; [

        # stockholm
        genid
        gnumake
        hashPassword
        lentil
        parallel
        (pkgs.writeScriptBin "im" ''
          #! ${pkgs.bash}/bin/bash
          export PATH=${makeSearchPath "bin" (with pkgs; [
            tmux
            gnugrep
            weechat
          ])}
          if tmux list-sessions -F\#S | grep -q '^im''$'; then
            exec tmux attach -t im
          else
            exec tmux new -s im weechat
          fi
        '')

        # root
        cryptsetup
        ntp # ntpate

        # tv
        bc
        bind # dig
        #cac
        dic
        file
        gnupg21
        haskellPackages.hledger
        htop
        jq
        manpages
        mkpasswd
        netcat
        nix-repl
        nmap
        nq
        p7zip
        pass
        posix_man_pages
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
    {
      tv.iptables = {
        enable = true;
        input-internet-accept-new-tcp = [
          "ssh"
          "http"
          "tinc"
          "smtp"
        ];
      };
    }
    {
      krebs.exim-retiolum.enable = true;
    }
    {
      krebs.nginx = {
        enable = true;
        servers.default.locations = [
          (nameValuePair "~ ^/~(.+?)(/.*)?\$" ''
            alias /home/$1/public_html$2;
          '')
        ];
      };
    }
    {
      krebs.retiolum = {
        enable = true;
        connectTo = [
          "cd"
          "gum"
          "pigstarter"
        ];
      };
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
      options = "defaults,noatime,ssd,compress=lzo";
    };
    "/home" = {
      device = "/dev/mapper/xuvga-home";
      fsType = "btrfs";
      options = "defaults,noatime,ssd,compress=lzo";
    };
    "/boot" = {
      device = "/dev/sda1";
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = "nosuid,nodev,noatime";
    };
  };

  nixpkgs.config.chromium.enablePepperFlash = true;

  nixpkgs.config.allowUnfree = true;
  #hardware.bumblebee.enable = true;
  #hardware.bumblebee.group = "video";
  hardware.enableAllFirmware = true;
  #hardware.opengl.driSupport32Bit = true;

  environment.systemPackages = with pkgs; [
    #xlibs.fontschumachermisc
    #slock
    ethtool
    #firefoxWrapper # with plugins
    #chromiumDevWrapper
    tinc
    iptables
    #jack2

    gptfdisk
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

  #virtualisation.libvirtd.enable = true;

  #services.bitlbee.enable = true;
  #services.tor.client.enable = true;
  #services.tor.enable = true;
  #services.virtualboxHost.enable = true;


  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";
}
