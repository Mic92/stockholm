{ config, lib, pkgs, ... }:

with config.krebs.lib;

{
  krebs.build.host = config.krebs.hosts.zu;

  imports = [
    {
      options.tv.test.sercret-file = mkOption {
        type = types.secret-file;
        default = {};
      };
    }
    ../.
    ../2configs/hw/x220.nix
    ../2configs/exim-retiolum.nix
    ../2configs/git.nix
    ../2configs/mail-client.nix
    ../2configs/man.nix
    ../2configs/nginx/public_html.nix
    ../2configs/pulse.nix
    ../2configs/retiolum.nix
    ../2configs/wu-binary-cache/client.nix
    ../2configs/xserver
    {
      environment.systemPackages = with pkgs; [

        # stockholm
        gnumake
        hashPassword
        haskellPackages.lentil
        parallel
        (pkgs.writeBashBin "im" ''
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

  nixpkgs.config.chromium.enablePepperFlash = true;

  #hardware.bumblebee.enable = true;
  #hardware.bumblebee.group = "video";
  hardware.enableAllFirmware = true;
  #hardware.opengl.driSupport32Bit = true;

  environment.systemPackages = with pkgs; [
    ethtool
    tinc_pre
    iptables
    #jack2

    gptfdisk
  ];

  security.setuidPrograms = [
    "sendmail"  # for cron
  ];

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

#/*
#{ host api.doraemon.sg.zalora.net | awk '{print$4" api.zalora.sg"}';
#  host bob.live.sg.zalora.net | awk '{print$4" bob.zalora.sg"}';
#  host www.live.sg.zalora.net | awk '{print$4" www.zalora.sg costa.zalora.sg"}'; }
#*/
#  networking.extraHosts = optionalString (1 == 1) ''
#54.255.133.72 api.zalora.sg
#52.77.12.194 bob.zalora.sg
#52.74.232.49 www.zalora.sg costa.zalora.sg
#  '';


  #services.elasticsearch.enable = true;
  #services.kibana.enable = true;
  #services.logstash.enable = true;

  environment.etc."ssh/ssh_config".text = mkForce ''
    AddressFamily ${if config.networking.enableIPv6 then "any" else "inet"}

    ${optionalString config.programs.ssh.setXAuthLocation ''
      XAuthLocation ${pkgs.xorg.xauth}/bin/xauth
    ''}

    ForwardX11 ${if config.programs.ssh.forwardX11 then "yes" else "no"}

    # Allow DSA keys for now. (These were deprecated in OpenSSH 7.0.)
    #PubkeyAcceptedKeyTypes +ssh-dss

    ${config.programs.ssh.extraConfig}
  '';

}
