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
    ./sqlBackup.nix
    (ssl [ "reich-gebaeudereinigung.de" "www.reich-gebaeudereinigung.de" ])
    (servePage [ "reich-gebaeudereinigung.de" "www.reich-gebaeudereinigung.de" ])

    (ssl [ "karlaskop.de" "www.karlaskop.de" ])
    (servePage [ "karlaskop.de" "www.karlaskop.de" ])

    (ssl [ "makeup.apanowicz.de" "www.makeup.apanowicz.de" ])
    (servePage [ "makeup.apanowicz.de" "www.makeup.apanowicz.de" ])

    (ssl [ "pixelpocket.de" "www.pixelpocket.de" ])
    (servePage [ "pixelpocket.de" "www.pixelpocket.de" ])

    (ssl [ "o.ubikmedia.de" "www.o.ubikmedia.de" ])
    (serveOwncloud [ "o.ubikmedia.de" "www.o.ubikmedia.de" ])

    (ssl [
      "ubikmedia.de"
      "aldona.ubikmedia.de"
      "apanowicz.de"
      "nirwanabluete.de"
      "aldonasiech.com"
      "360gradvideo.tv"
      "ubikmedia.eu"
      "www.ubikmedia.de"
      "www.aldona.ubikmedia.de"
      "www.apanowicz.de"
      "www.nirwanabluete.de"
      "www.aldonasiech.com"
      "www.360gradvideo.tv"
      "www.ubikmedia.eu"
    ])
    (serveWordpress [
      "ubikmedia.de"
      "apanowicz.de"
      "nirwanabluete.de"
      "aldonasiech.com"
      "360gradvideo.tv"
      "ubikmedia.eu"
      "www.apanowicz.de"
      "www.nirwanabluete.de"
      "www.aldonasiech.com"
      "www.360gradvideo.tv"
      "www.ubikmedia.eu"
      "*.ubikmedia.de"
    ])
  ];

  lass.mysqlBackup.config.all.databases = [
    "ubikmedia_de"
    "o_ubikmedia_de"
  ];

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

