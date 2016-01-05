{ config, lib, ... }:
with lib;
{
  krebs.backup.plans = addNames {
    xu-test-cd = {
      method = "push";
      src = { host = config.krebs.hosts.xu; path = "/tmp/xu-test"; };
      dst = { host = config.krebs.hosts.cd; path = "/tmp/backups/xu-test"; };
    };
    #xu-test-wu = {
    #  method = "push";
    #  dst = { user = tv; host = wu; path = "/krebs/backup/xu-test"; };
    #};
    cd-test-xu = {
      method = "pull";
      src = { host = config.krebs.hosts.cd; path = "/tmp/cd-test"; };
      dst = { host = config.krebs.hosts.xu; path = "/tmp/backups/cd-test"; };
    };

  };
}
