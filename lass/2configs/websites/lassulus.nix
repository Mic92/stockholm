{ config, pkgs, lib, ... }:

with lib;
let
  inherit (import <stockholm/lib>)
    genid
  ;
  inherit (import ../../4lib { inherit lib; }) initscript;

in {
  imports = [
    ../git.nix
  ];

  security.acme = {
    certs."lassul.us" = {
      email = "lass@lassul.us";
      webroot = "/var/lib/acme/acme-challenges";
      plugins = [
        "account_key.json"
        "key.pem"
        "fullchain.pem"
        "full.pem"
      ];
      allowKeysForGroup = true;
      group = "lasscert";
    };
    certs."cgit.lassul.us" = {
      email = "lassulus@gmail.com";
      webroot = "/var/lib/acme/acme-challenges";
      plugins = [
        "account_key.json"
        "key.pem"
        "fullchain.pem"
      ];
      group = "nginx";
      allowKeysForGroup = true;
    };
  };

  krebs.tinc_graphs.enable = true;

  users.users.lass-stuff = {
    uid = genid "lass-stuff";
    description = "lassul.us blog cgi stuff";
    home = "/var/empty";
  };

  services.phpfpm.poolConfigs."lass-stuff" = ''
    listen = /var/run/lass-stuff.socket
    user = lass-stuff
    group = nginx
    pm = dynamic
    pm.max_children = 5
    pm.start_servers = 1
    pm.min_spare_servers = 1
    pm.max_spare_servers = 1
    listen.owner = lass-stuff
    listen.group = nginx
    php_admin_value[error_log] = 'stderr'
    php_admin_flag[log_errors] = on
    catch_workers_output = yes
    security.limit_extensions =
  '';

  users.groups.lasscert.members = [
    "dovecot2"
    "ejabberd"
    "exim"
    "nginx"
  ];

  services.nginx.virtualHosts."lassul.us" = {
    serverAliases = [ "lassul.us" ];
    locations."/".extraConfig = ''
      root /srv/http/lassul.us;
    '';
    locations."/.well-known/acme-challenge".extraConfig = ''
      root /var/lib/acme/challenges/lassul.us/;
    '';
    locations."= /retiolum-hosts.tar.bz2".extraConfig = ''
      alias ${config.krebs.tinc.retiolum.hostsArchive};
    '';
    locations."/tinc".extraConfig = ''
      alias ${config.krebs.tinc_graphs.workingDir}/external;
    '';
    # TODO make this work!
    locations."= /ddate".extraConfig = let
      script = pkgs.writeBash "test" ''
        echo "hello world"
      '';
      #script = pkgs.execve "ddate-wrapper" {
      #  filename = "${pkgs.ddate}/bin/ddate";
      #  argv = [];
      #};
    in ''
      gzip off;
      fastcgi_pass unix:/var/run/lass-stuff.socket;
      include ${pkgs.nginx}/conf/fastcgi_params;
      fastcgi_param DOCUMENT_ROOT /var/empty;
      fastcgi_param SCRIPT_FILENAME ${script};
      fastcgi_param SCRIPT_NAME ${script};
    '';

    locations."/init".extraConfig = ''
      alias ${pkgs.writeText "init" (initscript { pubkey = config.krebs.users.lass.pubkey; })};
    '';

    enableSSL = true;
    extraConfig = "listen 80;";
    sslCertificate = "/var/lib/acme/lassul.us/fullchain.pem";
    sslCertificateKey = "/var/lib/acme/lassul.us/key.pem";
  };

  services.nginx.virtualHosts.cgit = {
    serverAliases = [
      "cgit.lassul.us"
    ];
    locations."/.well-known/acme-challenge".extraConfig = ''
      root /var/lib/acme/acme-challenges;
    '';
    enableSSL = true;
    extraConfig = "listen 80;";
    sslCertificate = "/var/lib/acme/cgit.lassul.us/fullchain.pem";
    sslCertificateKey = "/var/lib/acme/cgit.lassul.us/key.pem";
  };

  users.users.blog = {
    uid = genid "blog";
    description = "lassul.us blog deployment";
    home = "/srv/http/lassul.us";
    useDefaultShell = true;
    createHome = true;
    openssh.authorizedKeys.keys = [
      config.krebs.users.lass.pubkey
    ];
  };
}

