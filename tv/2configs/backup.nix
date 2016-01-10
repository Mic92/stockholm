{ config, lib, ... }:
with lib;
{
  krebs.backup.plans = addNames {
    wu-home-xu = {
      method = "push";
      src = { host = config.krebs.hosts.wu; path = "/home"; };
      dst = { host = config.krebs.hosts.xu; path = "/bku/wu-home"; };
      startAt = "05:00";
      snapshots = {
        daily    = { format = "%Y-%m-%d";    retain =  7; };
        weekly   = { format = "%YW%W";       retain =  4; };
        monthly  = { format = "%Y-%m";       retain = 12; };
        yearly   = { format = "%Y";                       };
      };
    };
  };
}
