{ config, lib, pkgs, ... }:
{
  services.postgresql = {
    enable = true;
    dataDir = "/var/state/postgresql/${config.services.postgresql.package.psqlSchema}";
    ensureDatabases = [ "atuin" ];
    ensureUsers = [{
      name = "atuin";
      ensurePermissions."DATABASE atuin" = "ALL PRIVILEGES";
    }];
  };
  systemd.tmpfiles.rules = [
    "d /var/state/postgresql 0700 postgres postgres -"
  ];
  users.groups.atuin = {};
  users.users.atuin = {
    uid = pkgs.stockholm.lib.genid_uint31 "atuin";
    isSystemUser = true;
    group = "atuin";
    home = "/run/atuin";
    createHome = true;
  };

  systemd.services.atuin = {
    wantedBy = [ "multi-user.target" ];
    environment = {
      ATUIN_HOST = "0.0.0.0";
      ATUIN_PORT = "8888";
      ATUIN_OPEN_REGISTRATION = "true";
      ATUIN_DB_URI = "postgres:///atuin";
    };
    serviceConfig = {
      User = "atuin";
      ExecStart = "${pkgs.atuin}/bin/atuin server start";
    };
  };
  networking.firewall.allowedTCPPorts = [ 8888 ];
}
