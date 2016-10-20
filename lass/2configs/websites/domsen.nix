{ config, pkgs, lib, ... }:

let

  inherit (import <stockholm/krebs/4lib> { config = {}; inherit lib; })
    genid
    genid_signed
  ;
  inherit (import <stockholm/lass/2configs/websites/util.nix> {inherit lib pkgs;})
    ssl
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

  check-password = pkgs.writeDash "check-password" ''
    read pw

    file="/home/$PAM_USER/.shadow"

    #check if shadow file exists
    test -e "$file" || exit 123

    hash="$(${pkgs.coreutils}/bin/head -1 $file)"
    salt="$(echo $hash | ${pkgs.gnused}/bin/sed 's/.*\$\(.*\)\$.*/\1/')"

    calc_hash="$(echo "$pw" | ${pkgs.mkpasswd}/bin/mkpasswd -m sha-512 -S $salt)"
    if [ "$calc_hash" == $hash ]; then
      exit 0
    else
      exit 1
    fi
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

    (ssl [ "pixelpocket.de" ])
    (servePage [ "pixelpocket.de" "www.pixelpocket.de" ])

    (ssl [ "o.ubikmedia.de" ])
    (serveOwncloud [ "o.ubikmedia.de" "www.o.ubikmedia.de" ])

    (ssl [
      "ubikmedia.de"
      "aldona.ubikmedia.de"
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
      "www.ubikmedia.de"
      "www.aldona.ubikmedia.de"
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
    ])
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
      "*.ubikmedia.de"
    ])
  ];

  krebs.nginx.servers."ubikmedia.de".locations = [
    (lib.nameValuePair "/piwik" ''
      try_files $uri $uri/ /index.php?$args;
    '')
  ];

  lass.mysqlBackup.config.all.databases = [
    "ubikmedia_de"
    "o_ubikmedia_de"
  ];

  krebs.backup.plans = {
    prism-sql-domsen = {
      method = "push";
      src = { host = config.krebs.hosts.prism;      path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.domsen-nas; path = "/mnt/UBIK-9TB-Pool/BACKUP/XXXX-MAX-UND-ANDERES/prism-sql"; };
      startAt = "00:01";
    };
    prism-http-domsen = {
      method = "push";
      src = { host = config.krebs.hosts.prism;      path = "/srv/http"; };
      dst = { host = config.krebs.hosts.domsen-nas; path = "/mnt/UBIK-9TB-Pool/BACKUP/XXXX-MAX-UND-ANDERES/prism-http"; };
      startAt = "00:10";
    };
    prism-o-ubikmedia-domsen = {
      method = "push";
      src = { host = config.krebs.hosts.prism;      path = "/srv/o.ubikmedia.de-data"; };
      dst = { host = config.krebs.hosts.domsen-nas; path = "/mnt/UBIK-9TB-Pool/BACKUP/XXXX-MAX-UND-ANDERES/prism-owncloud"; };
      startAt = "00:30";
    };
  };

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
    { predicate = "-p tcp --dport 465"; target = "ACCEPT"; }
  ];

  security.pam.services.exim.text = ''
    auth        required      pam_env.so
    auth        sufficient    pam_exec.so debug expose_authtok ${check-password}
    auth        sufficient    pam_unix.so likeauth nullok
    auth        required      pam_deny.so
    account     required      pam_unix.so
    password    required      pam_cracklib.so retry=3 type=
    password    sufficient    pam_unix.so nullok use_authtok md5shadow
    password    required      pam_deny.so
    session     required      pam_limits.so
    session     required      pam_unix.so
  '';

  krebs.exim-smarthost = {
    authenticators.PLAIN = ''
      driver = plaintext
      server_prompts = :
      server_condition = "''${if pam{$auth2:$auth3}{yes}{no}}"
      server_set_id = $auth2
    '';
    authenticators.LOGIN = ''
      driver = plaintext
      server_prompts = "Username:: : Password::"
      server_condition = "''${if pam{$auth1:$auth2}{yes}{no}}"
      server_set_id = $auth1
    '';
    internet-aliases = [
      { from = "dominik@apanowicz.de"; to = "dominik_a@gmx.de"; }
      { from = "mail@jla-trading.com"; to = "jla-trading"; }
      { from = "testuser@lassul.us"; to = "testuser"; }
    ];
    system-aliases = [
    ];
    ssl_cert = "/var/lib/acme/lassul.us/fullchain.pem";
    ssl_key = "/var/lib/acme/lassul.us/key.pem";
  };

  users.users.domsen = {
    uid = genid "domsen";
    description = "maintenance acc for domsen";
    home = "/home/domsen";
    useDefaultShell = true;
    extraGroups = [ "nginx" ];
    createHome = true;
  };

  users.users.jla-trading = {
    uid = genid "jla-trading";
    home = "/home/jla-trading";
    useDefaultShell = true;
    createHome = true;
  };
}

