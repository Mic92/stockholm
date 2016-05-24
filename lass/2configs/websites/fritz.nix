{ config, pkgs, lib, ... }:

let
  inherit (import ../../4lib { inherit lib pkgs; })
    manageCerts
    activateACME
    ssl
    servePage
    serveWordpress;

in {
  imports = [
    ( manageCerts [ "biostase.de" "www.biostase.de" ])
    #( serveWordpress [ "biostase.de" "www.biostase.de" ])

    ( manageCerts [ "radical-dreamers.de" ])
    ( serveWordpress [ "radical-dreamers.de" ])

    ( manageCerts [ "gs-maubach.de" ])
    ( serveWordpress [ "gs-maubach.de" ])

    ( manageCerts [ "spielwaren-kern.de" ])
    ( serveWordpress [ "spielwaren-kern.de" ])

    ( manageCerts [ "familienpraxis-korntal.de" ])
    ( servePage [ "familienpraxis-korntal.de" ])

    ( manageCerts [ "ttf-kleinaspach.de" ])
    ( serveWordpress [ "ttf-kleinaspach.de" ])

    ( ssl [ "eastuttgart.de" ])
    ( serveWordpress [ "eastuttgart.de" ])

    ( ssl [ "habsys.de" "habsys.eu" ])
    ( servePage [ "habsys.de" "habsys.eu" ])
  ];

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    rootPassword = toString (<secrets/mysql_rootPassword>);
  };

  lass.mysqlBackup = {
    enable = true;
    config.fritz = {
      password = toString (<secrets/mysql_rootPassword>);
      databases = [
        "biostase_de"
        "eastuttgart_de"
        "radical_dreamers_de"
        "spielwaren_kern_de"
        "ttf_kleinaspach_de"
      ];
    };
  };
}
