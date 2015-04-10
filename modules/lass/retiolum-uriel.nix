{ config, pkgs, ... }:

{
  imports = [
    ../tv/retiolum.nix
  ];

  services.retiolum = {
    enable = true;
    hosts = ../../hosts;
    privateKeyFile = "/etc/nixos/secrets/uriel.retiolum.rsa_key.priv";
    connectTo = [
      "fastpoke"
      "gum"
      "ire"
    ];
  };

  networking.firewall.allowedTCPPorts = [ 655 ];
  networking.firewall.allowedUDPPorts = [ 655 ];
}
