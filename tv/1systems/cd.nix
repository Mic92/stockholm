{ config, lib, pkgs, ... }:

with config.krebs.lib;

{
  krebs.build.host = config.krebs.hosts.cd;

  imports = [
    ../2configs/hw/CAC-Developer-2.nix
    ../2configs/fs/CAC-CentOS-7-64bit.nix
    ../2configs/exim-smarthost.nix
    ../2configs/git.nix
    ../2configs/retiolum.nix
    ../2configs/urlwatch.nix
    {
      imports = [ ../2configs/charybdis.nix ];
      tv.charybdis = {
        enable = true;
        sslCert = ../Zcerts/charybdis_cd.crt.pem;
      };
    }
    {
      tv.ejabberd = {
        enable = true;
        hosts = [ "jabber.viljetic.de" ];
      };
      tv.iptables.input-internet-accept-new-tcp = [
        "xmpp-client"
        "xmpp-server"
      ];
    }
    {
      krebs.github-hosts-sync.enable = true;
      tv.iptables.input-internet-accept-new-tcp =
        singleton config.krebs.github-hosts-sync.port;
    }
    {
      krebs.nginx.servers.cgit.server-names = [
        "cgit.cd.krebsco.de"
        "cgit.cd.viljetic.de"
      ];
      # TODO make public_html also available to cd, cd.retiolum (AKA default)
      krebs.nginx.servers.public_html = {
        server-names = singleton "cd.viljetic.de";
        locations = singleton (nameValuePair "~ ^/~(.+?)(/.*)?\$" ''
          alias /home/$1/public_html$2;
        '');
      };
      krebs.nginx.servers.viljetic = {
        server-names = singleton "viljetic.de";
        # TODO directly set root (instead via location)
        locations = singleton (nameValuePair "/" ''
          root ${pkgs.viljetic-pages};
        '');
      };
      tv.iptables.input-internet-accept-new-tcp = singleton "http";
    }
  ];

  networking.interfaces.enp2s1.ip4 = [
    {
      address = "162.219.7.216";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = "162.219.7.1";
  networking.nameservers = [
    "8.8.8.8"
  ];

  environment.systemPackages = with pkgs; [
    htop
    iftop
    iotop
    iptables
    nethogs
    ntp     # ntpate
    rxvt_unicode.terminfo
    tcpdump
  ];

  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';
}
