{ config, lib, ... }:
with config.krebs.lib;
{

  krebs.backup.plans = {
  } // mapAttrs (_: recursiveUpdate {
    snapshots = {
      daily    = { format = "%Y-%m-%d"; retain =  7; };
      weekly   = { format = "%YW%W";    retain =  4; };
      monthly  = { format = "%Y-%m";    retain = 12; };
      yearly   = { format = "%Y";                    };
    };
  }) {
    prism-chat-uriel = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/home/chat"; };
      dst = { host = config.krebs.hosts.uriel; path = "/bku/prism-chat"; };
      startAt = "03:00";
    };
    prism-chat-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/home/chat"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/prism-chat"; };
      startAt = "03:00";
    };
    mors-home-uriel = {
      method = "push";
      src = { host = config.krebs.hosts.mors;  path = "/home"; };
      dst = { host = config.krebs.hosts.uriel; path = "/bku/mors-home"; };
      startAt = "04:00";
    };
    uriel-home-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.uriel; path = "/home"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/uriel-home"; };
      startAt = "04:00";
    };
    prism-http-uriel = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/srv/http"; };
      dst = { host = config.krebs.hosts.uriel; path = "/bku/prism-http"; };
      startAt = "04:30";
    };
    prism-http-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/srv/http"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/prism-http"; };
      startAt = "04:30";
    };
    prism-sql-uriel = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.uriel; path = "/bku/prism-sql_dumps"; };
      startAt = "05:00";
    };
    prism-sql-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/prism-sql_dumps"; };
      startAt = "05:00";
    };
  };
}
