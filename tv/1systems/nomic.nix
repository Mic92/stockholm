{ config, lib, pkgs, ... }:

with lib;

{
  krebs.build.host = config.krebs.hosts.nomic;
  krebs.build.user = config.krebs.users.tv;

  krebs.build.target = "root@nomic.gg23";

  krebs.build.source = {
    git.nixpkgs = {
      url = https://github.com/4z3/nixpkgs;
      rev = "03130ec91356cd250b80f144022ee2f4d665ca36"; # 1357692
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
    ../2configs/AO753.nix
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
    cryptoModules = [ "aes" "sha1" "xts" ];
    devices = [
      {
        name = "luks1";
        device = "/dev/disk/by-uuid/cac73902-1023-4906-8e95-3a8b245337d4";
      }
    ];
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/de4780fc-0473-4708-81df-299b7383274c";
      fsType = "btrfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/be3a1d80-3157-4d7c-86cc-ef01b64eff5e";
      fsType = "ext4";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/9db9c8ff-51da-4cbd-9f0a-0cd3333bbaff";
      fsType = "btrfs";
    };

  swapDevices = [ ];

  nix = {
    buildCores = 2;
    maxJobs = 2;
    daemonIONiceLevel = 1;
    daemonNiceLevel = 1;
  };

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
