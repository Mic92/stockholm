{ config, pkgs, ... }: with import <stockholm/lib>;
{
  imports = [ <stockholm/krebs/2configs/syncthing.nix> ];
  services.syncthing = {
    group = "syncthing";
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 22000"; target = "ACCEPT";}
    { predicate = "-p udp --dport 21027"; target = "ACCEPT";}
  ];

  system.activationScripts.syncthing-home = mkDefault ''
    ${pkgs.coreutils}/bin/chmod a+x /home/lass
  '';
}
