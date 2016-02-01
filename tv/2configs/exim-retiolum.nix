{ lib, ... }:

with lib;

{
  krebs.exim-retiolum.enable = true;
  tv.iptables.input-retiolum-accept-new-tcp = singleton "smtp";
}
