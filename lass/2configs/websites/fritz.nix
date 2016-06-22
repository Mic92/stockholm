{ config, pkgs, lib, ... }:

with lib;
let
  inherit (import <stockholm/krebs/4lib> { config = {}; inherit lib; })
    genid
    head
  ;
  inherit (import <stockholm/lass/2configs/websites/util.nix> {inherit lib pkgs;})
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
    (ssl [ "biostase.de" "www.biostase.de" ])
    (serveWordpress [ "biostase.de" "www.biostase.de" ])

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
  ];

  lass.mysqlBackup.config.all.databases = [
    "biostase_de"
    "eastuttgart_de"
    "radical_dreamers_de"
    "spielwaren_kern_de"
    "ttf_kleinaspach_de"
  ];

  #password protect some dirs
  krebs.nginx.servers."biostase.de".locations = [
    (nameValuePair "/old_biostase.de" ''
      auth_basic "Administrator Login";
      auth_basic_user_file /srv/http/biostase.de/old_biostase.de/.htpasswd;
    '')
    (nameValuePair "/mysqldumper" ''
      auth_basic "Administrator Login";
      auth_basic_user_file /srv/http/biostase.de/mysqldumper/.htpasswd;
    '')
  ];

  users.users.root.openssh.authorizedKeys.keys = [
    config.krebs.users.fritz.pubkey
  ];

  services.phpfpm.phpIni = pkgs.runCommand "php.ini" {
     options = ''
      extension=${pkgs.phpPackages.apcu}/lib/php/extensions/apcu.so
      sendmail_path = "${sendmail} -t -i"
    '';
  } ''
    cat ${pkgs.php}/etc/php-recommended.ini > $out
    echo "$options" >> $out
  '';
}
