{ config, pkgs, lib, ... }:

let
  inherit (config.krebs.lib) genid;
  inherit (import ../../4lib { inherit lib pkgs; })
    manageCert
    manageCerts
    activateACME
    ssl
    servePage
    serveOwncloud
    serveWordpress;

in {
  imports = [
    ( ssl "reich-gebaeudereinigung.de" )
    ( servePage "reich-gebaeudereinigung.de" )

    ( manageCert "karlaskop.de" )
    ( servePage "karlaskop.de" )

    ( manageCert "makeup.apanowicz.de" )
    ( servePage "makeup.apanowicz.de" )

    ( manageCert "pixelpocket.de" )
    ( servePage "pixelpocket.de" )

    ( ssl "o.ubikmedia.de" )
    ( serveOwncloud "o.ubikmedia.de" )

    ( manageCerts [ "ubikmedia.de" "apanowicz.de" "nirwanabluete.de" "aldonasiech.com" "360gradvideo.tv" "ubikmedia.eu" ] )
    ( serveWordpress [ "ubikmedia.de" "*.ubikmedia.de" "apanowicz.de" "nirwanabluete.de" "aldonasiech.com" "360gradvideo.tv" "ubikmedia.eu" ] )
  ];

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    rootPassword = toString (<secrets/mysql_rootPassword>);
  };

  services.mysqlBackup = {
    enable = true;
    databases = [
      "ubikmedia_de"
      "o_ubikmedia_de"
    ];
    location = "/bku/sql_dumps";
  };

  users.users.domsen = {
    uid = genid "domsen";
    description = "maintenance acc for domsen";
    home = "/home/domsen";
    useDefaultShell = true;
    extraGroups = [ "nginx" ];
    createHome = true;
  };

  services.phpfpm.phpIni = pkgs.runCommand "php.ini" {
     options = ''
      extension=${pkgs.phpPackages.apcu}/lib/php/extensions/apcu.so
    '';
  } ''
    cat ${pkgs.php}/etc/php-recommended.ini > $out
    echo "$options" >> $out
  '';
}

