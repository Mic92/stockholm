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
    ( ssl [ "biostase.de" "www.biostase.de" ])
    ( serveWordpress [ "biostase.de" "www.biostase.de" ])

    ( ssl [ "radical-dreamers.de" "www.radical-dreamers.de" ])
    ( serveWordpress [ "radical-dreamers.de" "www.radical-dreamers.de" ])

    ( ssl [ "gs-maubach.de" "www.gs-maubach.de" ])
    ( serveWordpress [ "gs-maubach.de" "www.gs-maubach.de" ])

    ( ssl [ "spielwaren-kern.de" "www.spielwaren-kern.de" ])
    ( serveWordpress [ "spielwaren-kern.de" "www.spielwaren-kern.de" ])

    ( ssl [ "familienpraxis-korntal.de" "www.familienpraxis-korntal.de" ])
    ( servePage [ "familienpraxis-korntal.de" "www.familienpraxis-korntal.de" ])

    ( ssl [ "ttf-kleinaspach.de" "www.ttf-kleinaspach.de" ])
    ( serveWordpress [ "ttf-kleinaspach.de" "www.ttf-kleinaspach.de" ])

    ( ssl [ "eastuttgart.de" "www.eastuttgart.de" ])
    ( serveWordpress [ "eastuttgart.de" "www.eastuttgart.de" ])

    #( ssl [ "www.habsys.de" "habsys.de" "www.habsys.eu" "habsys.eu" ])
    ( servePage [ "www.habsys.de" "habsys.de" "www.habsys.eu" "habsys.eu" ])
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
  users.users.root.openssh.authorizedKeys.keys = [
    config.krebs.users.fritz.pubkey
  ];
}
