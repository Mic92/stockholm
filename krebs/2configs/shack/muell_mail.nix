{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/muell_mail";
      rev = "861ec25ab22797d8961efb32e72d79e113aa9f0f";
      sha256 = "sha256:18cw95zbr7isv4cw80cbpd84n5z208fwh5390i6j10jkn398mjq2";
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
