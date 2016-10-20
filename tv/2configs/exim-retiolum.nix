{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  krebs.exim-retiolum.enable = true;
  tv.iptables.input-retiolum-accept-tcp = singleton "smtp";
}
