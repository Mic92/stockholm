{ config, lib, pkgs, ... }:

with config.krebs.lib;

{
  krebs.build.host = config.krebs.hosts.cd;

  imports = [
    ../.
    ../2configs/hw/CAC-Developer-2.nix
    ../2configs/fs/CAC-CentOS-7-64bit.nix
    ../2configs/exim-smarthost.nix
    ../2configs/git.nix
    ../2configs/retiolum.nix
    ../2configs/urlwatch.nix
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
    {
      krebs.github-hosts-sync.enable = true;
      tv.iptables.input-internet-accept-tcp =
        singleton config.krebs.github-hosts-sync.port;
    }
    {
      krebs.nginx.servers.cgit.server-names = [
        "cgit.cd.krebsco.de"
        "cgit.cd.viljetic.de"
      ];
      # TODO make public_html also available to cd, cd.retiolum (AKA default)
      krebs.nginx.servers."https://viljetic.de" = {
        server-names = singleton "viljetic.de";
        listen = mkForce []; # disable default
        ssl = {
          enable = true;
          certificate = "/var/lib/acme/viljetic.de/fullchain.pem";
          certificate_key = "/var/lib/acme/viljetic.de/key.pem";
        };
        locations = [
          (nameValuePair "/" ''
            root ${pkgs.viljetic-pages};
          '')
          (nameValuePair "~ ^/~(.+?)(/.*)?\$" ''
            alias /home/$1/public_html$2;
          '')
        ];
      };
      krebs.nginx.servers."http://viljetic.de" = {
        server-names = singleton "viljetic.de";
        locations = [
          (nameValuePair "/.well-known/acme-challenge/" ''
            root /var/lib/acme/challenges/viljetic.de/;
          '')
          (nameValuePair "/" ''
            return 301 https://viljetic.de$request_uri;
          '')
        ];
      };
      security.acme = {
        certs."viljetic.de" = {
          email = "tomislav@viljetic.de";
          webroot = "/var/lib/acme/challenges/viljetic.de";
          plugins = [
            "account_key.json"
            "key.pem"
            "fullchain.pem"
          ];
          user = "nginx";
        };
      };
      tv.iptables.input-internet-accept-tcp = [
        "http"
        "https"
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
    ntp     # ntpate
    rxvt_unicode.terminfo
    tcpdump
  ];
}
