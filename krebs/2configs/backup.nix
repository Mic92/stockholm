{ config, lib, ... }:
with lib;
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
    wolf-share-puyak = {
      method = "pull";
      src = { host = config.krebs.hosts.wolf;  path = "/home/share"; };
      dst = { host = config.krebs.hosts.puyak; path = "/bku/wolf-share"; };
      startAt = "03:00";
    };
  };
}

