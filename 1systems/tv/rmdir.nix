{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    ../../2configs/tv/CAC-Developer-1.nix
    ../../2configs/tv/CAC-CentOS-7-64bit.nix
    ../../2configs/tv/base.nix
    ../../2configs/tv/consul-server.nix
    ../../2configs/tv/exim-smarthost.nix
    ../../2configs/tv/git-public.nix
    {
      imports = [ ../../2configs/tv/identity.nix ];
      tv.identity.self = config.tv.identity.hosts.rmdir;
    }
    {
      imports = [ ../../3modules/tv/iptables.nix ];
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
      imports = [ ../../3modules/krebs/retiolum.nix ];
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

  networking.hostName = "rmdir";
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
