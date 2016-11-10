{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  krebs.build.host = config.krebs.hosts.cd;

  imports = [
    ../.
    ../2configs/hw/CAC-Developer-2.nix
    ../2configs/fs/CAC-CentOS-7-64bit.nix
    ../2configs/exim-smarthost.nix
    ../2configs/retiolum.nix
    {
      tv.charybdis = {
        enable = true;
        ssl_cert = ../Zcerts/charybdis_cd.crt.pem;
      };
      tv.iptables.input-retiolum-accept-tcp = [
        config.tv.charybdis.port
        config.tv.charybdis.sslport
      ];
    }
    {
      tv.ejabberd = {
        enable = true;
        hosts = [ "jabber.viljetic.de" ];
      };
      tv.iptables.input-internet-accept-tcp = [
        "xmpp-client"
        "xmpp-server"
      ];
    }
  ];

  networking = {
    interfaces.enp2s1.ip4 = singleton {
      address = let
        addr = "45.62.237.203";
      in assert config.krebs.build.host.nets.internet.ip4.addr == addr; addr;
      prefixLength = 24;
    };
    defaultGateway = "45.62.237.1";
    nameservers = ["8.8.8.8"];
  };

  environment.systemPackages = with pkgs; [
    htop
    iftop
    iotop
    iptables
    nethogs
    rxvt_unicode.terminfo
    tcpdump
  ];
}
