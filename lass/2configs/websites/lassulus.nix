{ config, pkgs, lib, ... }:

with lib;
let
  inherit (import <stockholm/lib>)
    genid
  ;

in {
  imports = [
    ../git.nix
  ];

  security.acme = {
    certs."lassul.us" = {
      email = "lass@lassul.us";
      webroot = "/var/lib/acme/challenges/lassul.us";
      plugins = [
        "account_key.json"
        "key.pem"
        "fullchain.pem"
        "full.pem"
      ];
      allowKeysForGroup = true;
      group = "lasscert";
    };
    certs."cgit.lassul.us" = {
      email = "lassulus@gmail.com";
      webroot = "/var/lib/acme/challenges/cgit.lassul.us";
      plugins = [
        "account_key.json"
        "key.pem"
        "fullchain.pem"
      ];
      group = "nginx";
      allowKeysForGroup = true;
    };
  };

  users.groups.lasscert.members = [
    "dovecot2"
    "ejabberd"
    "exim"
    "nginx"
  ];

  krebs.nginx.servers."lassul.us" = {
    server-names = [ "lassul.us" ];
    locations = [
      (nameValuePair "/" ''
        root /srv/http/lassul.us;
      '')
      (nameValuePair "/.well-known/acme-challenge" ''
        root /var/lib/acme/challenges/lassul.us/;
      '')
    ];
    ssl = {
      enable = true;
      certificate = "/var/lib/acme/lassul.us/fullchain.pem";
      certificate_key = "/var/lib/acme/lassul.us/key.pem";
    };
  };

  krebs.nginx.servers.cgit = {
    server-names = [
      "cgit.lassul.us"
    ];
    locations = [
      (nameValuePair "/.well-known/acme-challenge" ''
        root /var/lib/acme/challenges/cgit.lassul.us/;
      '')
    ];
    ssl = {
      enable = true;
      certificate = "/var/lib/acme/cgit.lassul.us/fullchain.pem";
      certificate_key = "/var/lib/acme/cgit.lassul.us/key.pem";
    };
  };

  users.users.blog = {
    uid = genid "blog";
    description = "lassul.us blog deployment";
    home = "/srv/http/lassul.us";
    useDefaultShell = true;
    createHome = true;
    openssh.authorizedKeys.keys = [
      config.krebs.users.lass.pubkey
    ];
  };
}

