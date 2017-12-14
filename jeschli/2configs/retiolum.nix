{ config, pkgs, ... }:

{

  krebs.tinc.retiolum = {
    enable = true;
    connectTo = [
      "prism"
      "gum"
      "ni"
      "dishfire"
    ];
  };

  nixpkgs.config.packageOverrides = pkgs: {
    tinc = pkgs.tinc_pre;
  };

  environment.systemPackages = [
    pkgs.tinc
  ];
}
