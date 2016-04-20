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
    #( manageCerts [ "biostase.de" ])
    #( servePage [ "biostase.de" ])

    #( manageCerts [ "gs-maubach.de" ])
    #( servePage [ "gs-maubach.de" ])

    #( manageCerts [ "spielwaren-kern.de" ])
    #( servePage [ "spielwaren-kern.de" ])

    #( manageCerts [ "societyofsimtech.de" ])
    #( servePage [ "societyofsimtech.de" ])

    #( manageCerts [ "ttf-kleinaspach.de" ])
    #( servePage [ "ttf-kleinaspach.de" ])

    #( manageCerts [ "edsn.de" ])
    #( servePage [ "edsn.de" ])

    #( manageCerts [ "eab.berkeley.edu" ])
    #( servePage [ "eab.berkeley.edu" ])

    ( manageCerts [ "eastuttgart.de" ])
    ( serveWordpress [ "eastuttgart.de" ])

    ( manageCerts [ "habsys.de" "habsys.eu" ])
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
        "eastuttgart_de"
      ];
    };
  };

  #lass.owncloud = {
  #  "o.ubikmedia.de" = {
  #    instanceid = "oc8n8ddbftgh";
  #  };
  #};
}
