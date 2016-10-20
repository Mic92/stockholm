{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  krebs.exim-retiolum.enable = true;
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i retiolum -p tcp --dport smtp"; target = "ACCEPT"; }
  ];
}
