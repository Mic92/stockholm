{ config, lib, ... }:
with import <stockholm/lib>;
{

  # TODO add timerConfig to krebs.backup and randomize startup
  # TODO define plans more abstract
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
    dishfire-http-icarus = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/srv/http"; };
      dst = { host = config.krebs.hosts.icarus;   path = "/bku/dishfire-http"; };
      startAt = "03:10";
    };
    dishfire-http-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/srv/http"; };
      dst = { host = config.krebs.hosts.mors;     path = "/bku/dishfire-http"; };
      startAt = "03:05";
    };
    dishfire-http-shodan = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/srv/http"; };
      dst = { host = config.krebs.hosts.shodan;   path = "/bku/dishfire-http"; };
      startAt = "03:10";
    };
    dishfire-sql-prism = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.prism;    path = "/bku/dishfire-sql"; };
      startAt = "03:15";
    };
    dishfire-sql-icarus = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.icarus;   path = "/bku/dishfire-sql"; };
      startAt = "03:25";
    };
    dishfire-sql-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.mors;     path = "/bku/dishfire-sql"; };
      startAt = "03:20";
    };
    dishfire-sql-shodan = {
      method = "pull";
      src = { host = config.krebs.hosts.dishfire; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.shodan;   path = "/bku/dishfire-sql"; };
      startAt = "03:25";
    };
    prism-bitlbee-icarus = {
      method = "pull";
      src = { host = config.krebs.hosts.prism;  path = "/var/lib/bitlbee"; };
      dst = { host = config.krebs.hosts.icarus; path = "/bku/prism-bitlbee"; };
      startAt = "03:25";
    };
    prism-bitlbee-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/var/lib/bitlbee"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/prism-bitlbee"; };
      startAt = "03:25";
    };
    prism-bitlbee-shodan = {
      method = "pull";
      src = { host = config.krebs.hosts.prism;  path = "/var/lib/bitlbee"; };
      dst = { host = config.krebs.hosts.shodan; path = "/bku/prism-bitlbee"; };
      startAt = "03:25";
    };
    prism-chat-icarus = {
      method = "pull";
      src = { host = config.krebs.hosts.prism;  path = "/home/chat"; };
      dst = { host = config.krebs.hosts.icarus; path = "/bku/prism-chat"; };
      startAt = "03:35";
    };
    prism-chat-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/home/chat"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/prism-chat"; };
      startAt = "03:30";
    };
    prism-chat-shodan = {
      method = "pull";
      src = { host = config.krebs.hosts.prism;  path = "/home/chat"; };
      dst = { host = config.krebs.hosts.shodan; path = "/bku/prism-chat"; };
      startAt = "03:35";
    };
    prism-sql-icarus = {
      method = "pull";
      src = { host = config.krebs.hosts.prism;  path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.icarus; path = "/bku/prism-sql_dumps"; };
      startAt = "03:45";
    };
    prism-sql-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/prism-sql_dumps"; };
      startAt = "03:40";
    };
    prism-sql-shodan = {
      method = "pull";
      src = { host = config.krebs.hosts.prism;  path = "/bku/sql_dumps"; };
      dst = { host = config.krebs.hosts.shodan; path = "/bku/prism-sql_dumps"; };
      startAt = "03:45";
    };
    prism-http-icarus = {
      method = "pull";
      src = { host = config.krebs.hosts.prism;  path = "/srv/http"; };
      dst = { host = config.krebs.hosts.icarus; path = "/bku/prism-http"; };
      startAt = "03:55";
    };
    prism-http-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.prism; path = "/srv/http"; };
      dst = { host = config.krebs.hosts.mors;  path = "/bku/prism-http"; };
      startAt = "03:50";
    };
    prism-http-shodan = {
      method = "pull";
      src = { host = config.krebs.hosts.prism;  path = "/srv/http"; };
      dst = { host = config.krebs.hosts.shodan; path = "/bku/prism-http"; };
      startAt = "03:55";
    };
    icarus-home-mors = {
      method = "push";
      src = { host = config.krebs.hosts.icarus; path = "/home"; };
      dst = { host = config.krebs.hosts.mors;   path = "/bku/icarus-home"; };
      startAt = "05:00";
    };
    icarus-home-shodan = {
      method = "push";
      src = { host = config.krebs.hosts.icarus; path = "/home"; };
      dst = { host = config.krebs.hosts.shodan; path = "/bku/icarus-home"; };
      startAt = "05:00";
    };
    mors-home-icarus = {
      method = "push";
      src = { host = config.krebs.hosts.mors;   path = "/home"; };
      dst = { host = config.krebs.hosts.icarus; path = "/bku/mors-home"; };
      startAt = "05:00";
    };
    mors-home-shodan = {
      method = "push";
      src = { host = config.krebs.hosts.mors;   path = "/home"; };
      dst = { host = config.krebs.hosts.shodan; path = "/bku/mors-home"; };
      startAt = "05:00";
    };
    shodan-home-icarus = {
      method = "pull";
      src = { host = config.krebs.hosts.shodan; path = "/home"; };
      dst = { host = config.krebs.hosts.icarus; path = "/bku/shodan-home"; };
      startAt = "04:00";
    };
    shodan-home-mors = {
      method = "pull";
      src = { host = config.krebs.hosts.shodan; path = "/home"; };
      dst = { host = config.krebs.hosts.mors;   path = "/bku/shodan-home"; };
      startAt = "04:00";
    };
  };
}
