{ pkgs, ... }: let

  unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };

in {
  services.minecraft-server = {
    enable = true;
    eula = true;
    package = unstable.minecraft-server;
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 25565"; target = "ACCEPT"; }
    { predicate = "-p udp --dport 25565"; target = "ACCEPT"; }
  ];
}
