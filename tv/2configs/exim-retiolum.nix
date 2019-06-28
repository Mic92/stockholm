{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  environment.systemPackages = [
    pkgs.eximlog
  ];
  krebs.exim-retiolum.enable = true;
  krebs.exim-retiolum.rspamd.enable = config.krebs.build.host.name == "nomic";
  tv.iptables.input-retiolum-accept-tcp = singleton "smtp";
}
