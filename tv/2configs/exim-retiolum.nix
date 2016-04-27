{ config, lib, pkgs, ... }:

with config.krebs.lib;

{
  krebs.exim-retiolum.enable = true;
  tv.iptables.input-retiolum-accept-new-tcp = singleton "smtp";
}
