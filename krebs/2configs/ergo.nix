{ config, pkgs, ... }:

{
  networking.firewall.allowedTCPPorts = [
    6667
  ];

  krebs.ergo = {
    enable = true;
  };
}


