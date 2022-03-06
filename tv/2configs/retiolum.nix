{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  krebs.tinc.retiolum = {
    enable = true;
    connectTo = filter (ne config.krebs.build.host.name) [
      "ni"
      "prism"
      "eve"
    ];
    extraConfig = ''
      LocalDiscovery = yes
    '';
    tincPackage = pkgs.tinc_pre;
  };
  tv.iptables.input-internet-accept-tcp = singleton "tinc";
  tv.iptables.input-internet-accept-udp = singleton "tinc";
}
