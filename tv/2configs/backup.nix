{ config, lib, ... }:
with config.krebs.lib;
{
  krebs.backup.plans = {
    nomic-home-xu = {
      method = "push";
      src = { host = config.krebs.hosts.nomic; path = "/home"; };
      dst = { host = config.krebs.hosts.xu;    path = "/bku/nomic-home"; };
      startAt = "05:00";
      snapshots = {
        daily    = { format = "%Y-%m-%d";    retain =  7; };
        weekly   = { format = "%YW%W";       retain =  4; };
        monthly  = { format = "%Y-%m";       retain = 12; };
        yearly   = { format = "%Y";                       };
      };
    };
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
    xu-home-wu = {
      method = "push";
      src = { host = config.krebs.hosts.xu; path = "/home"; };
      dst = { host = config.krebs.hosts.wu; path = "/bku/xu-home"; };
      startAt = "06:00";
      snapshots = {
        daily    = { format = "%Y-%m-%d";    retain =  7; };
        weekly   = { format = "%YW%W";       retain =  4; };
        monthly  = { format = "%Y-%m";       retain = 12; };
        yearly   = { format = "%Y";                       };
      };
    };
  } // mapAttrs (_: recursiveUpdate {
    snapshots = {
      minutely = { format = "%Y-%m-%dT%H:%M"; retain = 3; };
      hourly   = { format = "%Y-%m-%dT%H";    retain = 3; };
      daily    = { format = "%Y-%m-%d";       retain = 3; };
    };
    startAt = null;
  }) {
    xu-test-push-xu = {
      method = "push";
      src = { host = config.krebs.hosts.xu; path = "/tmp/xu-bku-test-data"; };
      dst = { host = config.krebs.hosts.xu; path = "/bku/xu-test-push"; };
    };
    xu-test-pull-xu = {
      method = "pull";
      src = { host = config.krebs.hosts.xu; path = "/tmp/xu-bku-test-data"; };
      dst = { host = config.krebs.hosts.xu; path = "/bku/xu-test-pull"; };
    };
    xu-test-push-wu = {
      method = "push";
      src = { host = config.krebs.hosts.xu; path = "/tmp/xu-bku-test-data"; };
      dst = { host = config.krebs.hosts.wu; path = "/bku/xu-test-push"; };
    };
  };
}
