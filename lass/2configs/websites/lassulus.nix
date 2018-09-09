{ config, pkgs, lib, ... }:

with lib;
let
  inherit (import <stockholm/lib>)
    genid
  ;

in {
  imports = [
    ./default.nix
    ../git.nix
  ];

  security.acme = {
    certs."lassul.us" = {
      allowKeysForGroup = true;
      group = "lasscert";
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
    addSSL = true;
    enableACME = true;
    locations."/".extraConfig = ''
      root /srv/http/lassul.us;
    '';
    locations."= /retiolum-hosts.tar.bz2".extraConfig = ''
      alias ${config.krebs.tinc.retiolum.hostsArchive};
    '';
    locations."= /retiolum.hosts".extraConfig = ''
      alias ${pkgs.retiolum-hosts};
    '';
    locations."/tinc".extraConfig = ''
      alias ${config.krebs.tinc_graphs.workingDir}/external;
    '';
    locations."/urlaubyay2018".extraConfig = ''
      autoindex on;
      alias /srv/http/lassul.us-media/india2018;
      auth_basic "Restricted Content";
      auth_basic_user_file ${pkgs.writeText "pics-user-pass" ''
        paolo:$apr1$aQ6mYNR3$ho.aJ7icqSO.y.xKo3GQf0
      ''};
    '';
    locations."/heilstadt".extraConfig = ''
      autoindex on;
      alias /srv/http/lassul.us-media/grabowsee2018;
      auth_basic "Restricted Content";
      auth_basic_user_file ${pkgs.writeText "pics-user-pass" ''
        c-base:$apr1$aQ6mYNR3$ho.aJ7icqSO.y.xKo3GQf0
      ''};
    '';
    locations."/krebspage".extraConfig = ''
      default_type "text/html";
      alias ${pkgs.krebspage}/index.html;
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

    locations."/init".extraConfig = let
      initscript = pkgs.init.override {
        pubkey = config.krebs.users.lass.pubkey;
      };
    in ''
      alias ${initscript};
    '';
    locations."/pub".extraConfig = ''
      alias ${pkgs.writeText "pub" config.krebs.users.lass.pubkey};
    '';
  };

  security.acme.certs."cgit.lassul.us" = {
    email = "lassulus@lassul.us";
    webroot = "/var/lib/acme/acme-challenge";
    plugins = [
      "account_key.json"
      "fullchain.pem"
      "key.pem"
    ];
    group = "nginx";
    user = "nginx";
  };


  services.nginx.virtualHosts.cgit = {
    serverName = "cgit.lassul.us";
    addSSL = true;
    sslCertificate = "/var/lib/acme/cgit.lassul.us/fullchain.pem";
    sslCertificateKey = "/var/lib/acme/cgit.lassul.us/key.pem";
    locations."/.well-known/acme-challenge".extraConfig = ''
      root /var/lib/acme/acme-challenge;
    '';
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

