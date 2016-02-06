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
  krebs.build.host = config.krebs.hosts.rmdir;

  krebs.build.target = "root@rmdir.internet";

  imports = [
    ../2configs/hw/CAC-Developer-1.nix
    ../2configs/fs/CAC-CentOS-7-64bit.nix
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
      address = primary-addr4;
      prefixLength = 24;
    }
  ];
  # TODO define gateway in krebs/3modules/default.nix
  networking.defaultGateway = getDefaultGateway primary-addr4;

  networking.nameservers = [
    "8.8.8.8"
  ];

  environment.systemPackages = with pkgs; [
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
