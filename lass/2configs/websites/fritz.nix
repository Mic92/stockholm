{ config, pkgs, lib, ... }:

with lib;
let
  inherit (import <stockholm/lib>)
    genid
    head
  ;
  inherit (import <stockholm/lass/2configs/websites/util.nix> {inherit lib pkgs;})
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

  services.nginx.enable = true;

  imports = [
    ./default.nix
    ./sqlBackup.nix

    (serveWordpress [ "radical-dreamers.de" "www.radical-dreamers.de" ])

    (serveWordpress [ "gs-maubach.de" "www.gs-maubach.de" ])

    (serveWordpress [ "spielwaren-kern.de" "www.spielwaren-kern.de" ])

    (servePage [ "familienpraxis-korntal.de" "www.familienpraxis-korntal.de" ])

    (serveWordpress [ "ttf-kleinaspach.de" "www.ttf-kleinaspach.de" ])

    (serveWordpress [ "eastuttgart.de" "www.eastuttgart.de" ])

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
