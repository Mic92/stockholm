{ config, lib, pkgs, ... }:

with lib;

{
  krebs.build.host = config.krebs.hosts.nomic;
  krebs.build.user = config.krebs.users.tv;

  krebs.build.target = "root@nomic.gg23";

  krebs.build.source = {
    git.nixpkgs = {
      url = https://github.com/NixOS/nixpkgs;
      rev = "c44a593aa43bba6a0708f6f36065a514a5110613";
    };
    dir.secrets = {
      host = config.krebs.hosts.wu;
      path = "/home/tv/secrets/nomic";
    };
    dir.stockholm = {
      host = config.krebs.hosts.wu;
      path = "/home/tv/stockholm";
    };
  };

  imports = [
    ../2configs/hw/AO753.nix
    ../2configs/base.nix
    #../2configs/consul-server.nix
    ../2configs/git.nix
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
  ];

  boot.initrd.luks = {
    cryptoModules = [ "aes" "sha512" "xts" ];
    devices = [
      { name = "luks1"; device = "/dev/sda2"; }
    ];
  };

  fileSystems."/" =
    { device = "/dev/mapper/nomic1-root";
      fsType = "btrfs";
    };

  fileSystems."/boot" =
    { device = "/dev/sda1";
      fsType = "ext4";
    };

  fileSystems."/home" =
    { device = "/dev/mapper/nomic1-home";
      fsType = "btrfs";
    };

  swapDevices = [ ];

  # TODO base
  boot.tmpOnTmpfs = true;

  environment.systemPackages = with pkgs; [
    (writeScriptBin "play" ''
      #! /bin/sh
      set -euf
      mpv() { exec ${mpv}/bin/mpv "$@"; }
      case $1 in
        deepmix)      mpv http://deepmix.ru/deepmix128.pls;;
        groovesalad)  mpv http://somafm.com/play/groovesalad;;
        ntslive)      mpv http://listen2.ntslive.co.uk/listen.pls;;
        *)
          echo "$0: bad argument: $*" >&2
          exit 23
      esac
    '')
    gnupg
    ntp # ntpate
    rxvt_unicode.terminfo
    tmux
  ];
}
