{ config, pkgs, lib, ... }:

let
  inherit (config.krebs.lib) genid;
  inherit (import ../../4lib { inherit lib pkgs; })
    manageCert
    activateACME
    ssl
    servePage
    serveOwncloud;

in {
  imports = [
    ( ssl "reich-gebaeudereinigung.de" )
    ( servePage "reich-gebaeudereinigung.de" )

    ( servePage "karlaskop.de" )
    ( manageCert "karlaskop.de" )

    ( servePage "makeup.apanowicz.de" )
    ( manageCert "makeup.apanowicz.de" )

    ( servePage "pixelpocket.de" )
    ( manageCert "pixelpocket.de" )

    ( ssl "o.ubikmedia.de" )
    ( serveOwncloud "o.ubikmedia.de" )

  ];

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

