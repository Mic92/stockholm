with import ./lib;
{ config, pkgs, ... }: {
  krebs.backup.plans = {
  } // mapAttrs (_: recursiveUpdate {
    snapshots = {
      daily    = { format = "%Y-%m-%d"; retain =  7; };
      weekly   = { format = "%YW%W";    retain =  4; };
      monthly  = { format = "%Y-%m";    retain = 12; };
      yearly   = { format = "%Y";                    };
    };
  }) {
    bu-home-xu = {
      method = "push";
      src = { host = config.krebs.hosts.bu; path = "/home"; };
      dst = { host = config.krebs.hosts.xu; path = "/bku/bu-home"; };
      startAt = "05:20";
    };
    bu-home-zu = {
      method = "push";
      src = { host = config.krebs.hosts.bu; path = "/home"; };
      dst = { host = config.krebs.hosts.zu; path = "/bku/bu-home"; };
      startAt = "05:25";
    };
    nomic-home-xu = {
      method = "push";
      src = { host = config.krebs.hosts.nomic; path = "/home"; };
      dst = { host = config.krebs.hosts.xu;    path = "/bku/nomic-home"; };
      startAt = "05:00";
    };
    nomic-home-zu = {
      method = "push";
      src = { host = config.krebs.hosts.nomic; path = "/home"; };
      dst = { host = config.krebs.hosts.zu; path = "/bku/nomic-home"; };
      startAt = "04:20";
    };
    nomic-pull-querel-home = {
      method = "pull";
      src = { host = config.krebs.hosts.querel; path = "/home"; };
      dst = { host = config.krebs.hosts.nomic; path = "/fs/ponyhof/bku/querel-home"; };
      startAt = "22:00";
    };
    xu-home-bu = {
      method = "push";
      src = { host = config.krebs.hosts.xu; path = "/home"; };
      dst = { host = config.krebs.hosts.bu; path = "/bku/xu-home"; };
      startAt = "04:50";
    };
    xu-home-nomic = {
      method = "push";
      src = { host = config.krebs.hosts.xu; path = "/home"; };
      dst = { host = config.krebs.hosts.nomic; path = "/fs/cis3hG/bku/xu-home"; };
      startAt = "05:20";
    };
    xu-home-zu = {
      method = "push";
      src = { host = config.krebs.hosts.xu; path = "/home"; };
      dst = { host = config.krebs.hosts.zu; path = "/bku/xu-home"; };
      startAt = "06:20";
    };
    xu-pull-ni-ejabberd = {
      method = "pull";
      src = { host = config.krebs.hosts.ni; path = "/var/lib/ejabberd"; };
      dst = { host = config.krebs.hosts.xu; path = "/bku/ni-ejabberd"; };
      startAt = "07:00";
    };
    xu-pull-ni-home = {
      method = "pull";
      src = { host = config.krebs.hosts.ni; path = "/home"; };
      dst = { host = config.krebs.hosts.xu; path = "/bku/ni-home"; };
      startAt = "07:00";
    };
    zu-home-xu = {
      method = "push";
      src = { host = config.krebs.hosts.zu; path = "/home"; };
      dst = { host = config.krebs.hosts.xu; path = "/bku/zu-home"; };
      startAt = "05:00";
    };
    zu-pull-ni-ejabberd = {
      method = "pull";
      src = { host = config.krebs.hosts.ni; path = "/var/lib/ejabberd"; };
      dst = { host = config.krebs.hosts.zu; path = "/bku/ni-ejabberd"; };
      startAt = "06:00";
    };
    zu-pull-ni-home = {
      method = "pull";
      src = { host = config.krebs.hosts.ni; path = "/home"; };
      dst = { host = config.krebs.hosts.zu; path = "/bku/ni-home"; };
      startAt = "06:30";
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
  };
}
