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
    dishfire-http-prism = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/srv/http"; };
      dst = { host = config.krebs.hosts.prism;    path = "/bku/dishfire-http"; };
      startAt = "03:00";
    };
    dishfire-http-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/srv/http"; };
      dst = { host = config.krebs.hosts.mors;     path = "/bku/dishfire-http"; };
      startAt = "03:05";
    };
    dishfire-http-uriel = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/srv/http"; };
      dst = { host = config.krebs.hosts.uriel;    path = "/bku/dishfire-http"; };
      startAt = "03:10";
    };
    dishfire-sql-prism = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.prism;    path = "/bku/dishfire-sql"; };
      startAt = "03:15";
    };
    dishfire-sql-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.mors;     path = "/bku/dishfire-sql"; };
      startAt = "03:20";
    };
    dishfire-sql-uriel = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.uriel;    path = "/bku/dishfire-sql"; };
      startAt = "03:25";
    };
    prism-bitlbee-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/var/lib/bitlbee"; };
      dst = { host = config.krebs.hosts.mors; path = "/bku/prism-bitlbee"; };
      startAt = "03:25";
    };
    prism-bitlbee-uriel = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/var/lib/bitlbee"; };
      dst = { host = config.krebs.hosts.uriel; path = "/bku/prism-bitlbee"; };
      startAt = "03:25";
    };
    prism-chat-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/home/chat"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/prism-chat"; };
      startAt = "03:30";
    };
    prism-chat-uriel = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/home/chat"; };
      dst = { host = config.krebs.hosts.uriel; path = "/bku/prism-chat"; };
      startAt = "03:35";
    };
    prism-sql-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/prism-sql_dumps"; };
      startAt = "03:40";
    };
    prism-sql-uriel = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.uriel; path = "/bku/prism-sql_dumps"; };
      startAt = "03:45";
    };
    prism-http-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/srv/http"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/prism-http"; };
      startAt = "03:50";
    };
    prism-http-uriel = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/srv/http"; };
      dst = { host = config.krebs.hosts.uriel; path = "/bku/prism-http"; };
      startAt = "03:55";
    };
    uriel-home-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.uriel; path = "/home"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/uriel-home"; };
      startAt = "04:00";
    };
    mors-home-uriel = {
      method = "push";
      src = { host = config.krebs.hosts.mors;  path = "/home"; };
      dst = { host = config.krebs.hosts.uriel; path = "/bku/mors-home"; };
      startAt = "05:00";
    };
  };
}
