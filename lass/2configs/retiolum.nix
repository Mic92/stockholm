{ config, lib, pkgs, ... }:

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
    enable = true;
    connectTo = [
      "prism"
      "ni"
      "eve"
    ];
    extraConfig = ''
      StrictSubnets = yes
      ${lib.optionalString (config.krebs.build.host.nets.retiolum.via != null) ''
        LocalDiscovery = no
      ''}
    '';
  };

  nixpkgs.config.packageOverrides = pkgs: {
    tinc = pkgs.tinc_pre;
  };

  environment.systemPackages = [
    pkgs.tinc
  ];
}
