{ config, lib, ... }:

with config.krebs.lib;

{
  krebs.retiolum = {
    enable = true;
    connectTo = filter (ne config.krebs.build.host.name) [
      "gum"
      "prism"
      "echelon"
      "cd"
      "ire"
    ];
  };
  tv.iptables.input-internet-accept-tcp = singleton "tinc";
}
