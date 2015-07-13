{ config, pkgs, ... }:

{
  imports = [
    ../tv/retiolum
  ];

  tv.retiolum = {
    enable = true;
    hosts = <retiolum-hosts>;
    privateKeyFile = "/etc/nixos/secrets/mors.retiolum.rsa_key.priv";
    connectTo = [
      "fastpoke"
      "gum"
      "ire"
    ];
  };

  networking.firewall.allowedTCPPorts = [ 655 ];
  networking.firewall.allowedUDPPorts = [ 655 ];
}
