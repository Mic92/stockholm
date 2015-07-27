{ config, lib, pkgs, ... }:

with lib;

{
  krebs.build.host = config.krebs.hosts.rmdir;

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
