{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/s3-power";
      rev = "b2b87b56bb40d714dbbecd1285566870b256aec4";
      sha256 = "sha256:02wikwf3rgkkggwbwqisdvhlwd38w5pw011xhwvhnj114s3rynan";
    }) {};
    home = "/var/lib/s3-power";
    cfg = toString <secrets/shack/s3-power.json>;
in {
  users.users.s3_power = {
    inherit home;
    createHome = true;
  };
  systemd.services.s3-power = {
    startAt = "daily";
    description = "s3-power";
    environment.CONFIG = "${home}/s3-power.json";
    serviceConfig = {
      Type = "oneshot";
      User = "s3_power";
      ExecStartPre = pkgs.writeDash "s3-power-pre" ''
        install -D -os3_power -m700 ${cfg} ${home}/s3-power.json
      '';
      WorkingDirectory = home;
      PermissionsStartOnly = true;
      ExecStart = "${pkg}/bin/s3-power";
      PrivateTmp = true;
    };
  };
}
