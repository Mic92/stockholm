{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/s3-power";
      rev = "36df203a8fc1af02b08f60ab8d49c849b01e711f";
      sha256 = "sha256:0i05vllnfwj02sfpmg2m8hy0zq27kww9ampiaix6dl5wbyjlp51j";
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
