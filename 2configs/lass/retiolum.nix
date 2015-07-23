{ ... }:

{
  imports = [
    ../../3modules/lass/iptables.nix
    ../../3modules/tv/retiolum.nix
    ../../2configs/tv/exim-retiolum.nix
  ];

  lass.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport smtp"; target = "ACCEPT"; }
        { predicate = "-p tcp --dport tinc"; target = "ACCEPT"; }
        { predicate = "-p udp --dport tinc"; target = "ACCEPT"; }
      ];
    };
  };

  tv.retiolum = {
    enable = true;
    hosts = ../../Zhosts;
    connectTo = [
      "fastpoke"
      "cloudkrebs"
      "pigstarter"
    ];
  };
}
