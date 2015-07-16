{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    ../../2configs/tv/w110er.nix
    ../../2configs/tv/base.nix
    ../../2configs/tv/consul-client.nix
    ../../2configs/tv/exim-retiolum.nix
    ../../2configs/tv/git-public.nix
    # TODO git-private.nix
    ../../2configs/tv/mail-client.nix
    ../../2configs/tv/xserver.nix
    ../../2configs/tv/synaptics.nix # TODO w110er if xserver is enabled
    {
      imports = [ ../../3modules/tv/identity.nix ];
      tv.identity = {
        enable = true;
        self = config.tv.identity.hosts.wu;
      };
    }
    {
      environment.systemPackages = with pkgs; [

        # shitment
        git
        gnumake
        parallel

        # root
        cryptsetup
        ntp # ntpate

        # tv
        bind # dig
        file
        gitAndTools.qgit
        gnupg21
        haskellPackages.hledger
        htop
        jq
        manpages
        mkpasswd
        mpv
        netcat
        nix-repl
        nmap
        p7zip
        pavucontrol
        posix_man_pages
        qrencode
        sxiv
        texLive
        tmux
        weechat
        zathura

        #ack
        #apache-httpd
        #ascii
        #bc
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
        #pythonPackages.urlwatch
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
      ];
    }
    {
      imports = [ ../../3modules/tv/iptables.nix ];
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
      imports = [ ../../3modules/tv/nginx.nix ];
      tv.nginx = {
        enable = true;
        servers.default.locations = [
          (nameValuePair "~ ^/~(.+?)(/.*)?\$" ''
            alias /home/$1/public_html$2;
          '')
        ];
      };
    }
    {
      imports = [ ../../3modules/tv/retiolum.nix ];
      tv.retiolum = {
        enable = true;
        hosts = ../../Zhosts;
        connectTo = [
          "gum"
          "pigstarter"
        ];
      };
    }
    {
      imports = [ ../../3modules/tv/urlwatch.nix ];
      tv.urlwatch = {
        enable = true;
        mailto = "tv@wu.retiolum"; # TODO
        onCalendar = "*-*-* 05:00:00";
        urls = [
          ## nixpkgs maintenance

          # 2014-07-29 when one of the following urls change
          # then we have to update the package

          # ref src/nixpkgs/pkgs/tools/admin/sec/default.nix
          http://simple-evcorr.sourceforge.net/

          # ref src/nixpkgs/pkgs/tools/networking/urlwatch/default.nix
          https://thp.io/2008/urlwatch/

          # 2014-12-20 ref src/nixpkgs/pkgs/tools/networking/tlsdate/default.nix
          https://api.github.com/repos/ioerror/tlsdate/tags

          # 2015-02-18
          # ref ~/src/nixpkgs/pkgs/tools/text/qprint/default.nix
          http://www.fourmilab.ch/webtools/qprint/

          # 2014-09-24 ref https://github.com/4z3/xintmap
          http://www.mathstat.dal.ca/~selinger/quipper/

          # 2014-12-12 remove nixopsUnstable when nixops get's bumped to 1.3
          # ref https://github.com/NixOS/nixpkgs/blob/master/pkgs/tools/package-management/nixops/unstable.nix
          http://nixos.org/releases/nixops/

          ## other

          https://nixos.org/channels/nixos-unstable/git-revision

          ## 2014-10-17
          ## TODO update ~/src/login/default.nix
          #http://hackage.haskell.org/package/bcrypt
          #http://hackage.haskell.org/package/cron
          #http://hackage.haskell.org/package/hyphenation
          #http://hackage.haskell.org/package/iso8601-time
          #http://hackage.haskell.org/package/ixset-typed
          #http://hackage.haskell.org/package/system-command
          #http://hackage.haskell.org/package/transformers
          #http://hackage.haskell.org/package/web-routes-wai
          #http://hackage.haskell.org/package/web-page
        ];
      };
    }
    {
      users.extraGroups = {
        tv-sub.gid = 1337;
      };

      users.extraUsers =
        mapAttrs (name: user: user // {
          inherit name;
          home = "/home/${name}";
          createHome = true;
          useDefaultShell = true;
        }) {
          ff = {
            uid = 13378001;
            group = "tv-sub";
            extraGroups = [
              "audio"
              "video"
            ];
          };

          cr = {
            uid = 13378002;
            group = "tv-sub";
            extraGroups = [
              "audio"
              "video"
              "bumblebee"
            ];
          };

          vimb = {
            uid = 13378003;
            group = "tv-sub";
            extraGroups = [
              "audio"
              "video"
              "bumblebee"
            ];
          };

          fa = {
            uid = 2300001;
            group = "tv-sub";
          };

          rl = {
            uid = 2300002;
            group = "tv-sub";
          };

          tief = {
            uid = 2300702;
            group = "tv-sub";
          };

          btc-bitcoind = {
            uid = 2301001;
            group = "tv-sub";
          };

          btc-electrum = {
            uid = 2301002;
            group = "tv-sub";
          };

          ltc-litecoind = {
            uid = 2301101;
            group = "tv-sub";
          };

          eth = {
            uid = 2302001;
            group = "tv-sub";
          };

          emse-hsdb = {
            uid = 4200101;
            group = "tv-sub";
          };

          wine = {
            uid = 13370400;
            group = "tv-sub";
            extraGroups = [
              "audio"
              "video"
              "bumblebee"
            ];
          };

          # dwarffortress
          df = {
            uid = 13370401;
            group = "tv-sub";
            extraGroups = [
              "audio"
              "video"
              "bumblebee"
            ];
          };

          # XXX visudo: Warning: Runas_Alias `FTL' referenced but not defined
          FTL = {
            uid = 13370402;
            #group = "tv-sub";
            extraGroups = [
              "audio"
              "video"
              "bumblebee"
            ];
          };

          freeciv = {
            uid = 13370403;
            group = "tv-sub";
          };

          xr = {
            uid = 13370061;
            group = "tv-sub";
            extraGroups = [
              "audio"
              "video"
            ];
          };

          "23" = {
            uid = 13370023;
            group = "tv-sub";
          };

          electrum = {
            uid = 13370102;
            group = "tv-sub";
          };

          Reaktor = {
            uid = 4230010;
            group = "tv-sub";
          };

          gitolite = {
            uid = 7700;
          };

          skype = {
            uid = 6660001;
            group = "tv-sub";
            extraGroups = [
              "audio"
            ];
          };

          onion = {
            uid = 6660010;
            group = "tv-sub";
          };

          zalora = {
            uid = 1000301;
            group = "tv-sub";
            extraGroups = [
              "audio"
              # TODO remove vboxusers when hardening is active
              "vboxusers"
              "video"
            ];
          };
        };

      security.sudo.extraConfig =
        let
          inherit (import ../../4lib/tv { inherit lib pkgs; })
            isSuffixOf;

          hasMaster = { group ? "", ... }:
            isSuffixOf "-sub" group;

          masterOf = user : removeSuffix "-sub" user.group;
        in
        concatStringsSep "\n"
          (map (u: "${masterOf u} ALL=(${u.name}) NOPASSWD: ALL")
               (filter hasMaster (attrValues config.users.extraUsers)));
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
      options = "defaults,noatime,ssd,compress=lzo";
    };
    "/home" = {
      device = "/dev/mapper/home";
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

  nixpkgs.config.firefox.enableAdobeFlash = true;
  nixpkgs.config.chromium.enablePepperFlash = true;

  nixpkgs.config.allowUnfree = true;
  hardware.bumblebee.enable = true;
  hardware.bumblebee.group = "video";
  hardware.enableAllFirmware = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.enable = true;

  networking.hostName = "wu";

  environment.systemPackages = with pkgs; [
    xlibs.fontschumachermisc
    slock
    ethtool
    #firefoxWrapper # with plugins
    #chromiumDevWrapper
    tinc
    iptables
    #jack2
  ];

  security.setuidPrograms = [
    "sendmail"  # for cron
    "slock"
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

  virtualisation.libvirtd.enable = true;

  networking.extraHosts = ''
    192.168.1.1 wrt.gg23 wrt
    192.168.1.11 mors.gg23
    192.168.1.12 uriel.gg23
    192.168.1.23 raspi.gg23 raspi
    192.168.1.37 wu.gg23
    192.168.1.111 nomic.gg23
    192.168.1.124 schnabeldrucker.gg23 schnabeldrucker
  '';

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="00:90:f5:da:aa:c3", NAME="en0"
    SUBSYSTEM=="net", ATTR{address}=="a0:88:b4:1b:ae:6c", NAME="wl0"

    # for jack
    KERNEL=="rtc0", GROUP="audio"
    KERNEL=="hpet", GROUP="audio"
  '';

  services.bitlbee.enable = true;
  services.tor.client.enable = true;
  services.tor.enable = true;
  services.virtualboxHost.enable = true;

  # TODO w110er if xserver is enabled
  services.xserver.vaapiDrivers = [ pkgs.vaapiIntel ];
}
