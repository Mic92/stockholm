{ config, pkgs, ... }:

{

  krebs.iptables = {
    tables = {
      filter.INPUT.rules = let
        tincport = toString config.krebs.build.host.nets.retiolum.tinc.port;
      in [
        { predicate = "-p tcp --dport ${tincport}"; target = "ACCEPT"; }
        { predicate = "-p udp --dport ${tincport}"; target = "ACCEPT"; }
      ];
    };
  };

  krebs.tinc.retiolum = {
    enableLegacy = true;
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
