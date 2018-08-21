{ config, pkgs, ... }:

{

  nixpkgs.config.steam.java = true;
  environment.systemPackages = with pkgs; [
    steam
  ];
  hardware.opengl.driSupport32Bit = true;

  #ports for inhome streaming
  krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport 27031"; target = "ACCEPT"; }
        { predicate = "-p tcp --dport 27036"; target = "ACCEPT"; }
        { predicate = "-p udp --dport 27031"; target = "ACCEPT"; }
        { predicate = "-p udp --dport 27036"; target = "ACCEPT"; }
      ];
    };
  };
}
