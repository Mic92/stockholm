{ config, pkgs, ... }:

{
  imports = [
    ../3modules/go.nix
  ];
  environment.systemPackages = [
    pkgs.go
  ];
  lass.go = {
    enable = true;
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i retiolum -p tcp --dport 1337"; target = "ACCEPT"; }
  ];
}
