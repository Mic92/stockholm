{ ... }:

{

  krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport smtp"; target = "ACCEPT"; }
        { predicate = "-p tcp --dport tinc"; target = "ACCEPT"; }
        { predicate = "-p udp --dport tinc"; target = "ACCEPT"; }
      ];
    };
  };

  krebs.retiolum = {
    enable = true;
    connectTo = [
      "prism"
      "pigstarter"
      "gum"
      "flap"
    ];
  };
}
