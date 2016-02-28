{ config, lib, pkgs, ... }:

with config.krebs.lib;

{
  krebs.exim-retiolum.enable = true;
  krebs.setuid.sendmail = {
    filename = "${pkgs.exim}/bin/exim";
    mode = "4111";
  };
  tv.iptables.input-retiolum-accept-new-tcp = singleton "smtp";
}
