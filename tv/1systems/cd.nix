{ config, lib, pkgs, ... }:

with lib;

{
  krebs.build.host = config.krebs.hosts.cd;
  krebs.build.user = config.krebs.users.tv;

  krebs.build.target = "root@cd.internet";

  krebs.build.source = {
    git.nixpkgs = {
      url = https://github.com/4z3/nixpkgs;
      rev = "03130ec91356cd250b80f144022ee2f4d665ca36"; # 1357692
    };
    dir.secrets = {
      host = config.krebs.hosts.wu;
      path = "/home/tv/secrets/cd";
    };
    dir.stockholm = {
      host = config.krebs.hosts.wu;
      path = "/home/tv/stockholm";
    };
  };

  imports = [
    ../2configs/hw/CAC-Developer-2.nix
    ../2configs/fs/CAC-CentOS-7-64bit.nix
    ../2configs/base.nix
    #../2configs/consul-server.nix
    ../2configs/exim-smarthost.nix
    ../2configs/git.nix
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
    }
    {
      krebs.github-hosts-sync.enable = true;
      tv.iptables.input-internet-accept-new-tcp =
        singleton config.krebs.github-hosts-sync.port;
    }
    {
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
      tv.iptables.input-internet-accept-new-tcp = singleton "http";
      krebs.nginx.servers.cgit.server-names = [
        "cgit.cd.krebsco.de"
        "cgit.cd.viljetic.de"
      ];
    }
    {
      # TODO make public_html also available to cd, cd.retiolum (AKA default)
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
          root ${pkgs.viljetic-pages};
        '');
      };
    }
    {
      krebs.retiolum = {
        enable = true;
        connectTo = [
          "fastpoke"
          "pigstarter"
          "ire"
        ];
      };
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
    mutt    # for mv
    nethogs
    ntp     # ntpate
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
      openssh.authorizedKeys.keys = [
        config.krebs.users.mv.pubkey
      ];
    };
  };
}
