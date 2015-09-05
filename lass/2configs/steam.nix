{ config, pkgs, ... }:

{

  imports = [
    ./games.nix
  ];
  #
  # Steam stuff
  # source: https://nixos.org/wiki/Talk:Steam
  #
  ##TODO: make steam module
  hardware.opengl.driSupport32Bit = true;

  nixpkgs.config.steam.java = true;
  environment.systemPackages = with pkgs; [
    steam
  ];
  lass.iptables = {
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
