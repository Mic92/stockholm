{ config, pkgs, lib, ... }:

let

  inherit (import <stockholm/lib>)
    genid
    genid_signed
  ;
  inherit (import <stockholm/lass/2configs/websites/util.nix> {inherit lib pkgs;})
    servePage
    serveOwncloud
    serveWordpress;

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
    (servePage [ "reich-gebaeudereinigung.de" "www.reich-gebaeudereinigung.de" ])

    (servePage [ "karlaskop.de" "www.karlaskop.de" ])

    (servePage [ "makeup.apanowicz.de" "www.makeup.apanowicz.de" ])

    (servePage [ "pixelpocket.de" ])

    (serveOwncloud [ "o.ubikmedia.de" ])

    (serveWordpress [
      "ubikmedia.de"
      "apanowicz.de"
      "nirwanabluete.de"
      "aldonasiech.com"
      "360gradvideo.tv"
      "ubikmedia.eu"
      "facts.cloud"
      "youthtube.xyz"
      "illucloud.eu"
      "illucloud.de"
      "illucloud.com"
      "www.apanowicz.de"
      "www.nirwanabluete.de"
      "www.aldonasiech.com"
      "www.360gradvideo.tv"
      "www.ubikmedia.eu"
      "www.facts.cloud"
      "www.youthtube.xyz"
      "www.illucloud.eu"
      "www.illucloud.de"
      "www.illucloud.com"
      "www.ubikmedia.de"
      "apanowicz.ubikmedia.de"
      "karlaskop.ubikmedia.de"
      "nb.ubikmedia.de"
      "cinevita.ubikmedia.de"
      "factscloud.ubikmedia.de"
      "youthtube.ubikmedia.de"
      "aldona2.ubikmedia.de"
      "illucloud.ubikmedia.de"
    ])
  ];

  services.nginx.virtualHosts."ubikmedia.de".locations."/piwika".extraConfig = ''
    try_files $uri $uri/ /index.php?$args;
  '';

  lass.mysqlBackup.config.all.databases = [
    "ubikmedia_de"
    "o_ubikmedia_de"
  ];

  services.phpfpm.phpOptions = ''
    sendmail_path = ${sendmail} -t
    upload_max_filesize = 100M
    post_max_size = 100M
    file_uploads = on
  '';

  # MAIL STUFF
  # TODO: make into its own module
  services.dovecot2 = {
    enable = true;
    mailLocation = "maildir:~/Mail";
    sslServerCert = "/var/lib/acme/lassul.us/fullchain.pem";
    sslServerKey = "/var/lib/acme/lassul.us/key.pem";
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport pop3s"; target = "ACCEPT"; }
    { predicate = "-p tcp --dport imaps"; target = "ACCEPT"; }
  ];

  krebs.exim-smarthost = {
    authenticators.PLAIN = ''
      driver = plaintext
      public_name = PLAIN
      server_condition = ''${run{${config.lass.usershadow.path}/bin/verify_arg ${config.lass.usershadow.pattern} $auth2 $auth3}{yes}{no}}
    '';
    authenticators.LOGIN = ''
      driver = plaintext
      public_name = LOGIN
      server_prompts = "Username:: : Password::"
      server_condition = ''${run{${config.lass.usershadow.path}/bin/verify_arg ${config.lass.usershadow.pattern} $auth1 $auth2}{yes}{no}}
    '';
    internet-aliases = [
      { from = "dominik@apanowicz.de"; to = "dominik_a@gmx.de"; }
      { from = "mail@jla-trading.com"; to = "jla-trading"; }
    ];
    sender_domains = [
      "jla-trading.com"
    ];
    ssl_cert = "/var/lib/acme/lassul.us/fullchain.pem";
    ssl_key = "/var/lib/acme/lassul.us/key.pem";
  };

  users.users.domsen = {
    uid = genid "domsen";
    description = "maintenance acc for domsen";
    home = "/home/domsen";
    useDefaultShell = true;
    extraGroups = [ "nginx" "download" ];
    createHome = true;
  };

  users.users.jla-trading = {
    uid = genid "jla-trading";
    home = "/home/jla-trading";
    useDefaultShell = true;
    createHome = true;
  };
}

