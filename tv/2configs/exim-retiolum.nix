{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  environment.systemPackages = [
    pkgs.eximlog
  ];
  krebs.exim-retiolum.enable = true;
  tv.iptables.input-retiolum-accept-tcp = singleton "smtp";
}
