{ config, pkgs, lib, ... }:

with lib;
let
  inherit (import <stockholm/lib>)
    genid
  ;

  servephpBB = domains:
    let
      domain = head domains;

    in {
      services.nginx.virtualHosts."${domain}" = {
        enableACME = true;
        forceSSL = true;
        serverAliases = domains;
        extraConfig = ''
          index index.php;
          root /srv/http/${domain}/;
          access_log /tmp/nginx_acc.log;
          error_log /tmp/nginx_err.log;
          error_page 404 /404.html;
          error_page 500 502 503 504 /50x.html;
          client_max_body_size 100m;
        '';
        locations."/".extraConfig = ''
          try_files $uri $uri/ /index.php?$args;
        '';
        locations."~ \.php(?:$|/)".extraConfig =  ''
          fastcgi_split_path_info ^(.+\.php)(/.+)$;
          include ${pkgs.nginx}/conf/fastcgi_params;
          fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
          fastcgi_param PATH_INFO $fastcgi_path_info;
          fastcgi_param HTTPS on;
          fastcgi_param modHeadersAvailable true; #Avoid sending the security headers twice
          fastcgi_pass unix:/srv/http/${domain}/phpfpm.pool;
          fastcgi_intercept_errors on;
        '';
        #Directives to send expires headers and turn off 404 error logging.
        locations."~* ^.+\.(xml|ogg|ogv|svg|svgz|eot|otf|woff|mp4|ttf|css|rss|atom|js|jpg|jpeg|gif|png|ico|zip|tgz|gz|rar|bz2|doc|xls|exe|ppt|tar|mid|midi|wav|bmp|rtf)$".extraConfig = ''
          access_log off;
          log_not_found off;
          expires max;
        '';
      };
      services.phpfpm.poolConfigs."${domain}" = ''
        listen = /srv/http/${domain}/phpfpm.pool
        user = nginx
        group = nginx
        pm = dynamic
        pm.max_children = 25
        pm.start_servers = 5
        pm.min_spare_servers = 3
        pm.max_spare_servers = 20
        listen.owner = nginx
        listen.group = nginx
        php_admin_value[error_log] = 'stderr'
        php_admin_flag[log_errors] = on
        catch_workers_output = yes
      '';
    };

in {
  imports = [
    ./default.nix
    ../git.nix
    (servephpBB [ "rote-allez-fraktion.de" ])
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
    serverAliases = [ "lassul.us" ];
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
  };

  services.nginx.virtualHosts.cgit = {
    addSSL = true;
    enableACME = true;
    serverAliases = [
      "cgit.lassul.us"
    ];
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

