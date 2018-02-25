{ pkgs, ... }:

{
  users.users = {
    mc = {
      name = "mc";
      description = "user playing mc";
      home = "/home/mc";
      createHome = true;
      useDefaultShell = true;
      packages = with pkgs; [
        tmux
      ];
    };
  };
  krebs.per-user.mc.packages = [ pkgs.jdk ];
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 25565"; target = "ACCEPT"; }
    { predicate = "-p udp --dport 25565"; target = "ACCEPT"; }
    { predicate = "-p tcp --dport 8123"; target = "ACCEPT"; }
  ];
}
