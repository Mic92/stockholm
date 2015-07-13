{ config, pkgs, ... }:

{
  imports = [
    ../tv/retiolum
    ../lass/iptables
  ];

  tv.retiolum = {
    enable = true;
    hosts = ../../hosts;
    privateKeyFile = "/etc/nixos/secrets/uriel.retiolum.rsa_key.priv";
    connectTo = [
      "fastpoke"
      "gum"
      "ire"
    ];
  };

  #networking.firewall.allowedTCPPorts = [ 655 ];
  #networking.firewall.allowedUDPPorts = [ 655 ];
  #lass.iptables = {
  #  #input-internet-accept-new-tcp = [ "tinc" ];
  #  #input-internet-accept-new-udp = [ "tinc" ];
  #  tables.retiolum = {
  #     interfaces = [ "retiolum" "wl0" ];
  #     allowed-tcp = [ "tinc" ];
  #     allowed-udp = [ "tinc" ];
  #  };
  #};
}
