{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/muell_mail";
      rev = "57b67c95052d90044137b2c89007a371dc389afd";
      sha256 = "1grkzs6fxjnc2bv4kskj63d5sb4qxz6yyr85nj0da9hn7qkk4jkj";
    }) { mkYarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage; };
    home = "/var/lib/muell_mail";
    cfg = toString <secrets/shack/muell_mail.js>;
in {
  users.users.muell_mail = {
    inherit home;
    createHome = true;
  };
  systemd.services.muell_mail = {
    description = "muell_mail";
    wantedBy = [ "multi-user.target" ];
    environment.CONFIG = "${home}/muell_mail.js";
    serviceConfig = {
      User = "muell_mail";
      ExecStartPre = pkgs.writeDash "muell_mail-pre" ''
        install -D -omuell_mail -m700 ${cfg} ${home}/muell_mail.js
      '';
      WorkingDirectory = home;
      PermissionsStartOnly = true;
      ExecStart = "${pkg}/bin/muell_mail";
      PrivateTmp = true;
      Restart = "always";
      RestartSec = "15";
    };
  };
}
