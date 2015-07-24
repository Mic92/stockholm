{ config, lib, pkgs, ... }:

with lib;

let
  Zpkgs = import ../../Zpkgs/tv { inherit pkgs; };
in

{
  imports = [
    ../../2configs/tv/CAC-Developer-2.nix
    ../../2configs/tv/CAC-CentOS-7-64bit.nix
    ../../2configs/tv/base.nix
    ../../2configs/tv/consul-server.nix
    ../../2configs/tv/exim-smarthost.nix
    ../../2configs/tv/git-public.nix
    {
      imports = [ ../../2configs/tv/charybdis.nix ];
      tv.charybdis = {
        enable = true;
        sslCert = ../../Zcerts/charybdis_cd.crt.pem;
      };
    }
    {
      imports = [ ../../3modules/tv/ejabberd.nix ];
      tv.ejabberd = {
        enable = true;
        hosts = [ "jabber.viljetic.de" ];
      };
    }
    {
      imports = [ ../../3modules/tv/github-hosts-sync.nix ];
      tv.github-hosts-sync.enable = true;
      tv.iptables.input-internet-accept-new-tcp =
        singleton config.tv.github-hosts-sync.port;
    }
    {
      imports = [ ../../2configs/tv/identity.nix ];
      tv.identity.self = config.tv.identity.hosts.cd;
    }
    {
      imports = [ ../../3modules/tv/iptables.nix ];
      tv.iptables = {
        enable = true;
        input-internet-accept-new-tcp = [
          "ssh"
          "tinc"
          "smtp"
          "xmpp-client"
          "xmpp-server"
        ];
        input-retiolum-accept-new-tcp = [
          "http"
        ];
      };
    }
    {
      imports = [
        ../../3modules/tv/iptables.nix
        ../../3modules/krebs/nginx.nix
      ];
      tv.iptables.input-internet-accept-new-tcp = singleton "http";
      krebs.nginx.servers.cgit.server-names = singleton "cgit.cd.viljetic.de";
    }
    {
      # TODO make public_html also available to cd, cd.retiolum (AKA default)
      imports = [
        ../../3modules/tv/iptables.nix
        ../../3modules/krebs/nginx.nix
      ];
      tv.iptables.input-internet-accept-new-tcp = singleton "http";
      krebs.nginx.servers.public_html = {
        server-names = singleton "cd.viljetic.de";
        locations = singleton (nameValuePair "~ ^/~(.+?)(/.*)?\$" ''
          alias /home/$1/public_html$2;
        '');
      };
    }
    {
      krebs.nginx.servers.viljetic = {
        server-names = singleton "viljetic.de";
        # TODO directly set root (instead via location)
        locations = singleton (nameValuePair "/" ''
          root ${Zpkgs.viljetic-pages};
        '');
      };
    }
    {
      imports = [ ../../3modules/tv/retiolum.nix ];
      tv.retiolum = {
        enable = true;
        hosts = ../../Zhosts;
        connectTo = [
          "fastpoke"
          "pigstarter"
          "ire"
        ];
      };
    }
  ];

  networking.hostName = "cd";
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
    git # required for ./deploy, clone_or_update
    htop
    iftop
    iotop
    iptables
    mutt    # for mv
    nethogs
    rxvt_unicode.terminfo
    tcpdump
  ];

  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';

  users.extraUsers = {
    mv = {
      uid = 1338;
      group = "users";
      home = "/home/mv";
      createHome = true;
      useDefaultShell = true;
      openssh.authorizedKeys.keys = map readFile [
        ../../Zpubkeys/mv_vod.ssh.pub
      ];
    };
  };
}
