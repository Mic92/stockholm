with import ./lib;
{ config, pkgs, ... }: {
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
    tincUp = lib.mkIf config.systemd.network.enable "";
  };
  systemd.network.networks.retiolum = {
    matchConfig.Name = "retiolum";
    address = let
      inherit (config.krebs.build.host.nets.retiolum) ip4 ip6;
    in [
      "${ip4.addr}/${toString ip4.prefixLength}"
      "${ip6.addr}/${toString ip6.prefixLength}"
    ];
  };
  tv.iptables.input-internet-accept-tcp = singleton "tinc";
  tv.iptables.input-internet-accept-udp = singleton "tinc";
}
