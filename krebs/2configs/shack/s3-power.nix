{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/s3-power";
      rev = "1a59f8e34924c8809d06895bd96c7f98d037026e";
      sha256 = "sha256:191625mg7n41852h1c0ay3492f29n7kxkab0kwczyp07xh5y25nn";
    }) {};
    home = "/var/lib/s3-power";
    cfg = toString <secrets/shack/s3-power.json>;
in {
  users.users.s3_power = {
    inherit home;
    createHome = true;
  };
  systemd.services.s3-power = {
    description = "s3-power";
    wantedBy = [ "multi-user.target" ];
    environment.CONFIG = "${home}/s3-power.json";
    serviceConfig = {
      User = "s3_power";
      ExecStartPre = pkgs.writeDash "s3-power-pre" ''
        install -D -os3_power -m700 ${cfg} ${home}/s3-power.json
      '';
      WorkingDirectory = home;
      PermissionsStartOnly = true;
      ExecStart = "${pkg}/bin/s3-power";
      Restart = "always";
      PrivateTmp = true;
    };
  };
}
