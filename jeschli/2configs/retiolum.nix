{ config, pkgs, ... }:

{

  krebs.tinc.retiolum = {
    enable = true;
    connectTo = [
      "prism"
      "gum"
      "ni"
      "dishfire"
      "enklave"
    ];
  };

  nixpkgs.config.packageOverrides = pkgs: {
    tinc = pkgs.tinc_pre;
  };

  networking.firewall.allowedTCPPorts = [ 655 ];
  networking.firewall.allowedUDPPorts = [ 655 ];

  environment.systemPackages = [
    pkgs.tinc
  ];
}
