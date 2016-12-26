{ config, pkgs, lib, ... }:

with lib;
let
  inherit (import <stockholm/lib>)
    genid
    head
  ;
  inherit (import <stockholm/lass/2configs/websites/util.nix> {inherit lib pkgs;})
    manageCerts
    ssl
    servePage
    serveWordpress
  ;

  msmtprc = pkgs.writeText "msmtprc" ''
    account localhost
      host localhost
    account default: localhost
  '';

  sendmail = pkgs.writeDash "msmtp" ''
    exec ${pkgs.msmtp}/bin/msmtp --read-envelope-from -C ${msmtprc} "$@"
  '';

in {
  imports = [
    ./sqlBackup.nix

    (ssl [ "radical-dreamers.de" "www.radical-dreamers.de" ])
    (serveWordpress [ "radical-dreamers.de" "www.radical-dreamers.de" ])

    (ssl [ "gs-maubach.de" "www.gs-maubach.de" ])
    (serveWordpress [ "gs-maubach.de" "www.gs-maubach.de" ])

    (ssl [ "spielwaren-kern.de" "www.spielwaren-kern.de" ])
    (serveWordpress [ "spielwaren-kern.de" "www.spielwaren-kern.de" ])

    (ssl [ "familienpraxis-korntal.de" "www.familienpraxis-korntal.de" ])
    (servePage [ "familienpraxis-korntal.de" "www.familienpraxis-korntal.de" ])

    (ssl [ "ttf-kleinaspach.de" "www.ttf-kleinaspach.de" ])
    (serveWordpress [ "ttf-kleinaspach.de" "www.ttf-kleinaspach.de" ])

    (ssl [ "eastuttgart.de" "www.eastuttgart.de" ])
    (serveWordpress [ "eastuttgart.de" "www.eastuttgart.de" ])

    (ssl [ "habsys.de" "www.habsys.de" "habsys.eu" "www.habsys.eu" ])
    (servePage [ "habsys.de" "www.habsys.de" "habsys.eu" "www.habsys.eu" ])

    (manageCerts [ "goldbarrendiebstahl.radical-dreamers.de" ])
    (serveWordpress [ "goldbarrendiebstahl.radical-dreamers.de" ])
  ];

  lass.mysqlBackup.config.all.databases = [
    "eastuttgart_de"
    "radical_dreamers_de"
    "spielwaren_kern_de"
    "ttf_kleinaspach_de"
  ];

  users.users.root.openssh.authorizedKeys.keys = [
    config.krebs.users.fritz.pubkey
  ];

  users.users.goldbarrendiebstahl = {
    home = "/srv/http/goldbarrendiebstahl.radical-dreamers.de";
    uid = genid "goldbarrendiebstahl";
    createHome = true;
    useDefaultShell = true;
    openssh.authorizedKeys.keys = [
      config.krebs.users.fritz.pubkey
    ];
  };

  services.phpfpm.phpOptions = ''
    sendmail_path = ${sendmail} -t
  '';
}
