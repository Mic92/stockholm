{ config, lib, pkgs, ... }:

with config.krebs.lib;

{
  krebs.exim-retiolum.enable = true;
  krebs.setuid.sendmail = {
    filename = "${pkgs.exim}/bin/exim";
    mode = "4111";
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i retiolum -p tcp --dport smtp"; target = "ACCEPT"; }
  ];
}
