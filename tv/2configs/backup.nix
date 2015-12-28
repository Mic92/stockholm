{ config, lib, ... }:
with lib;
{
  krebs.backup.plans = addNames {
    xu-test-cd = {
      method = "push";

      src = { host = config.krebs.hosts.xu; path = "/tmp/xu-test"; };
      dst = { host = config.krebs.hosts.cd; path = "/tmp/backups/xu-test"; };

      #startAt = "0,6,12,18:00";
      startAt = "minutely";
      snapshots = {
        minutely = { format = "%Y-%m-%dT%H:%M"; retain =  5; };
        hourly   = { format = "%Y-%m-%dT%H";    retain =  4; };
        daily    = { format = "%Y-%m-%d";       retain =  7; };
        weekly   = { format = "%YW%W";          retain =  4; };
        monthly  = { format = "%Y-%m";          retain = 12; };
        yearly   = { format = "%Y";                          };
      };
    };
    #xu-test-wu = {
    #  method = "push";
    #  dst = { user = tv; host = wu; path = "/krebs/backup/xu-test"; };
    #};
    cd-test-xu = {
      method = "pull";
      src = { host = config.krebs.hosts.cd; path = "/tmp/cd-test"; };
      dst = { host = config.krebs.hosts.xu; path = "/tmp/backups/cd-test"; };
      startAt = "minutely";
      snapshots = {
        minutely = { format = "%Y-%m-%dT%H:%M"; retain =  5; };
        hourly   = { format = "%Y-%m-%dT%H";    retain =  4; };
        daily    = { format = "%Y-%m-%d";       retain =  7; };
        weekly   = { format = "%YW%W";          retain =  4; };
        monthly  = { format = "%Y-%m";          retain = 12; };
        yearly   = { format = "%Y";                          };
      };
    };

  };
}
