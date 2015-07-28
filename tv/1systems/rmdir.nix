{ config, lib, pkgs, ... }:

with lib;

{
  krebs.build.host = config.krebs.hosts.rmdir;
  krebs.build.user = config.krebs.users.tv;

  krebs.build.target = "root@rmdir.internet";

  krebs.build.deps = {
    nixpkgs = {
      url = https://github.com/NixOS/nixpkgs;
      rev = "4c01e6d91993b6de128795f4fbdd25f6227fb870";
    };
    secrets = {
      url = "/home/tv/secrets/${config.krebs.build.host.name}";
    };
    stockholm = {
      url = toString ../..;
    };
  };

  imports = [
    ../2configs/CAC-Developer-1.nix
    ../2configs/CAC-CentOS-7-64bit.nix
    ../2configs/base.nix
    ../2configs/consul-server.nix
    ../2configs/exim-smarthost.nix
    ../2configs/git.nix
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
          "mkdir"
          "fastpoke"
          "pigstarter"
          "ire"
        ];
      };
    }
  ];

  networking.interfaces.enp2s1.ip4 = [
    {
      address = "167.88.44.94";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = "167.88.44.1";
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
