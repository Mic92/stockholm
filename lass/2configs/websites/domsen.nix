{ config, pkgs, ... }:

let
  inherit (config.krebs.lib) genid;
in {
  imports = [
    ../../3modules/static_nginx.nix
    ../../3modules/owncloud_nginx.nix
    ../../3modules/wordpress_nginx.nix
  ];

  lass.staticPage = {
    "karlaskop.de" = {};
    "makeup.apanowicz.de" = {};
    "pixelpocket.de" = {};
    "reich-gebaeudereinigung.de" = {};
  };

  lass.owncloud = {
    "o.ubikmedia.de" = {
      instanceid = "oc8n8ddbftgh";
    };
  };

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    rootPassword = toString (<secrets/mysql_rootPassword>);
  };

  users.users.domsen = {
    uid = genid "domsen";
    description = "maintenance acc for domsen";
    home = "/home/domsen";
    useDefaultShell = true;
    extraGroups = [ "nginx" ];
    createHome = true;
  };

  #lass.wordpress = {
  #  "ubikmedia.de" = {
  #  };
  #};

}

