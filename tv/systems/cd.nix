{ config, lib, pkgs, ... }:

with lib;

let
  tvpkgs = import ../pkgs { inherit pkgs; };
in

{
  krebs.build.host = config.krebs.hosts.cd;
  krebs.build.user = config.krebs.users.tv;

  krebs.build.target = "root@cd.internet";

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
    ../configs/CAC-Developer-2.nix
    ../configs/CAC-CentOS-7-64bit.nix
    ../configs/base.nix
    ../configs/consul-server.nix
    ../configs/exim-smarthost.nix
    ../configs/git.nix
    {
      imports = [ ../configs/charybdis.nix ];
      tv.charybdis = {
        enable = true;
        sslCert = ../../Zcerts/charybdis_cd.crt.pem;
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
      krebs.nginx.servers.cgit.server-names = singleton "cgit.cd.viljetic.de";
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
          root ${tvpkgs.viljetic-pages};
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
      openssh.authorizedKeys.keys = [
        config.krebs.users.mv.pubkey
      ];
    };
  };
}
