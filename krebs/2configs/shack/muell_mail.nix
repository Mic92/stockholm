{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchFromGitHub {
      owner = "shackspace";
      repo = "muell_mail";
      rev = "c3e43687879f95e01a82ef176fa15678543b2eb8";
      sha256 = "0hgchwam5ma96s2v6mx2jfkh833psadmisjbm3k3153rlxp46frx";
    }) { mkYarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage; };
    home = "/var/lib/muell_mail";
    cfg = "${config.krebs.secret.directory}/shack/muell_mail.js";
in {
  users.users.muell_mail = {
    inherit home;
    isSystemUser = true;
    createHome = true;
    group = "muell_mail";
  };
  users.groups.muell_mail = {};
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
