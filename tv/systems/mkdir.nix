{ config, lib, pkgs, ... }:

with lib;

{
  krebs.build.host = config.krebs.hosts.mkdir;
  krebs.build.user = config.krebs.users.tv;

  krebs.build.target = "root@mkdir.internet";

  krebs.build.deps = {
    nixpkgs = {
      url = https://github.com/NixOS/nixpkgs;
      rev = "9d5508d85c33b8fb22d79dde6176792eac2c2696";
    };
    secrets = {
      url = "/home/tv/secrets/${config.krebs.build.host.name}";
    };
    stockholm = {
      url = toString ../..;
    };
  };

  imports = [
    ../configs/CAC-Developer-1.nix
    ../configs/CAC-CentOS-7-64bit.nix
    ../configs/base.nix
    ../configs/consul-server.nix
    ../configs/exim-smarthost.nix
    ../configs/git.nix
    {
      tv.iptables = {
        enable = true;
        input-internet-accept-new-tcp = [
          "ssh"
          "tinc"
          "smtp"
        ];
        input-retiolum-accept-new-tcp = [
          "http"
        ];
      };
    }
    {
      krebs.retiolum = {
        enable = true;
        connectTo = [
          "cd"
          "fastpoke"
          "pigstarter"
          "ire"
        ];
      };
    }
  ];

  networking.interfaces.enp2s1.ip4 = [
    {
      address = "162.248.167.241"; # TODO
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = "162.248.167.1";
  networking.nameservers = [
    "8.8.8.8"
  ];

  environment.systemPackages = with pkgs; [
    git # required for ./deploy, clone_or_update
    htop
    iftop
    iotop
    iptables
    nethogs
    rxvt_unicode.terminfo
    tcpdump
  ];

  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';
}
