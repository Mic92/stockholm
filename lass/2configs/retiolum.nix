{ ... }:

{

  krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport tinc"; target = "ACCEPT"; }
        { predicate = "-p udp --dport tinc"; target = "ACCEPT"; }
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
}
