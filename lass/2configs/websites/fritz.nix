{ config, pkgs, ... }:

{

  imports = [
    ../../3modules/static_nginx.nix
    ../../3modules/owncloud_nginx.nix
    ../../3modules/wordpress_nginx.nix
  ];

  lass.staticPage = {
    "biostase.de" = {};
    "gs-maubach.de" = {};
    "spielwaren-kern.de" = {};
    "societyofsimtech.de" = {};
    "ttf-kleinaspach.de" = {};
    "edsn.de" = {};
    "eab.berkeley.edu" = {};
    "habsys.de" = {};
  };

  #lass.owncloud = {
  #  "o.ubikmedia.de" = {
  #    instanceid = "oc8n8ddbftgh";
  #  };
  #};

  #services.mysql = {
  #  enable = true;
  #  package = pkgs.mariadb;
  #  rootPassword = toString (<secrets/mysql_rootPassword>);
  #};
}
