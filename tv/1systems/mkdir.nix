{ config, lib, pkgs, ... }:

with lib;

let
  # TODO merge with lass
  getDefaultGateway = ip:
    concatStringsSep "." (take 3 (splitString "." ip) ++ ["1"]);


  primary-addr4 =
    builtins.elemAt config.krebs.build.host.nets.internet.addrs4 0;

  #secondary-addr4 =
  #  builtins.elemAt config.krebs.build.host.nets.internet.addrs4 1;
in

{
  krebs.build.host = config.krebs.hosts.mkdir;
  krebs.build.user = config.krebs.users.tv;

  krebs.build.target = "root@${primary-addr4}";

  krebs.build.source = {
    git.nixpkgs = {
      url = https://github.com/NixOS/nixpkgs;
      rev = "68bd8e4a9dc247726ae89cc8739574261718e328";
    };
    dir.secrets = {
      host = config.krebs.hosts.wu;
      path = "/home/tv/secrets/mkdir";
    };
    dir.stockholm = {
      host = config.krebs.hosts.wu;
      path = "/home/tv/stockholm";
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
          "fastpoke"
          "pigstarter"
          "ire"
        ];
      };
    }
  ];

  networking.interfaces.enp2s1.ip4 = [
    {
      address = primary-addr4;
      prefixLength = 24;
    }
    #{
    #  address = secondary-addr4;
    #  prefixLength = 24;
    #}
  ];

  # TODO define gateway in krebs/3modules/default.nix
  networking.defaultGateway = getDefaultGateway primary-addr4;

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
