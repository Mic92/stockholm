{ config, lib, pkgs, ... }:

with lib;

{
  krebs.build.host = config.krebs.hosts.wu;

  imports = [
    ../2configs/hw/w110er.nix
    #../2configs/consul-client.nix
    ../2configs/git.nix
    ../2configs/mail-client.nix
    ../2configs/xserver
    ../2configs/z.nix
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
        cac
        dic
        ff
        file
        get
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
        nq
        p7zip
        pavucontrol
        posix_man_pages
        pssh
        push
        qrencode
        sxiv
        texLive
        tmux
        zathura

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
          "gum"
          "pigstarter"
        ];
      };
    }
    {
      users.extraGroups = {
        tv.gid = 1337;
        slaves.gid = 3799582008; # genid slaves
      };

      users.extraUsers =
        mapAttrs (name: user@{ extraGroups ? [], ... }: user // {
          inherit name;
          home = "/home/${name}";
          createHome = true;
          useDefaultShell = true;
          group = "tv";
          extraGroups = ["slaves"] ++ extraGroups;
        }) {
          ff = {
            uid = 13378001;
            extraGroups = [
              "audio"
              "video"
            ];
          };

          cr = {
            uid = 13378002;
            extraGroups = [
              "audio"
              "video"
            ];
          };

          fa = {
            uid = 2300001;
          };

          rl = {
            uid = 2300002;
          };

          tief = {
            uid = 2300702;
          };

          btc-bitcoind = {
            uid = 2301001;
          };

          btc-electrum = {
            uid = 2301002;
          };

          ltc-litecoind = {
            uid = 2301101;
          };

          eth = {
            uid = 2302001;
          };

          emse-hsdb = {
            uid = 4200101;
          };

          wine = {
            uid = 13370400;
            extraGroups = [
              "audio"
              "video"
            ];
          };

          df = {
            uid = 13370401;
            extraGroups = [
              "audio"
              "video"
            ];
          };

          xr = {
            uid = 13370061;
            extraGroups = [
              "audio"
              "video"
            ];
          };

          "23" = {
            uid = 13370023;
          };

          electrum = {
            uid = 13370102;
          };

          skype = {
            uid = 6660001;
            extraGroups = [
              "audio"
            ];
          };

          onion = {
            uid = 6660010;
          };
        };

      security.sudo.extraConfig =
        let
          isSlave = u: elem "slaves" u.extraGroups;
          masterOf = u: u.group;
          slaves = filterAttrs (_: isSlave) config.users.extraUsers;
          toSudoers = u: "${masterOf u} ALL=(${u.name}) NOPASSWD: ALL";
        in
        concatMapStringsSep "\n" toSudoers (attrValues slaves);
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
  hardware.bumblebee.enable = true;
  hardware.bumblebee.group = "video";
  hardware.enableAllFirmware = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.enable = true;

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

}
