{ config, lib, pkgs, ... }:

{
  krebs.secret.files.mysql_rootPassword = {
    path = "${config.services.mysql.dataDir}/mysql_rootPassword";
    owner.name = "mysql";
    source-path = toString <secrets> + "/mysql_rootPassword";
  };

  services.mysql = {
    enable = true;
    dataDir = "/var/mysql";
    package = pkgs.mariadb;
    rootPassword = config.krebs.secret.files.mysql_rootPassword.path;
  };

  systemd.services.mysql = {
    requires = [ "secret.service" ];
    after = [ "secret.service" ];
  };

  lass.mysqlBackup = {
    enable = true;
    config.all = {
      password = toString (<secrets/mysql_rootPassword>);
    };
  };
}

