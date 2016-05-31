{ config, pkgs, lib, ... }:

let
  inherit (import <stockholm/krebs/4lib> { config = {}; inherit lib; })
    genid
    ;
  inherit (import <stockholm/lass/2configs/websites/util.nix> {inherit lib pkgs;})
    ssl
    servePage
    serveOwncloud
    serveWordpress;

  msmtprc = pkgs.writeText "msmtprc" ''
    account prism
      host localhost
    account default: prism
  '';

  sendmail = pkgs.writeDash "msmtp" ''
    exec ${pkgs.msmtp}/bin/msmtp --read-envelope-from -C ${msmtprc} "$@"
  '';

in {
  imports = [
    ( ssl [ "reich-gebaeudereinigung.de" ])
    ( servePage [ "reich-gebaeudereinigung.de" ])

    ( manageCerts [ "karlaskop.de" ])
    ( servePage [ "karlaskop.de" ])

    ( ssl [ "makeup.apanowicz.de" ])
    ( servePage [ "makeup.apanowicz.de" ])

    ( manageCerts [ "pixelpocket.de" ])
    ( servePage [ "pixelpocket.de" ])

    ( ssl [ "o.ubikmedia.de" ])
    ( serveOwncloud [ "o.ubikmedia.de" ])

    ( ssl [ "ubikmedia.de" "aldona.ubikmedia.de" "apanowicz.de" "nirwanabluete.de" "aldonasiech.com" "360gradvideo.tv" "ubikmedia.eu" ] )
    ( serveWordpress [ "ubikmedia.de" "*.ubikmedia.de" "apanowicz.de" "nirwanabluete.de" "aldonasiech.com" "360gradvideo.tv" "ubikmedia.eu" ] )
  ];

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    rootPassword = toString (<secrets/mysql_rootPassword>);
  };

  lass.mysqlBackup = {
    enable = true;
    config.domsen = {
      password = toString (<secrets/mysql_rootPassword>);
      databases = [
        "ubikmedia_de"
        "o_ubikmedia_de"
      ];
    };
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

  #services.phpfpm.phpOptions = ''
  #  extension=${pkgs.phpPackages.apcu}/lib/php/extensions/apcu.so
  #  sendmail_path = ${sendmail} -t
  #'';
  services.phpfpm.phpIni = pkgs.runCommand "php.ini" {
     options = ''
      extension=${pkgs.phpPackages.apcu}/lib/php/extensions/apcu.so
      sendmail_path = ${sendmail} -t -i"
    '';
  } ''
    cat ${pkgs.php}/etc/php-recommended.ini > $out
    echo "$options" >> $out
  '';
}

