{ config, ... }:

{
  services.teamspeak3 = {
    enable = true;
  };

  krebs.iptables.tables.filter.INPUT.rules = [
    #voice port
    { predicate = "-p tcp --dport 9987"; target = "ACCEPT"; }
    { predicate = "-p udp --dport 9987"; target = "ACCEPT"; }
    ##file transfer port
    #{ predicate = "-p tcp --dport 30033"; target = "ACCEPT"; }
    #{ predicate = "-p udp --dport 30033"; target = "ACCEPT"; }
    ##query port
    #{ predicate = "-p tcp --dport 10011"; target = "ACCEPT"; }
    #{ predicate = "-p udp --dport 10011"; target = "ACCEPT"; }
  ];
}
