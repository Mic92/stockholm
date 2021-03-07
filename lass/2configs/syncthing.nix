{ config, pkgs, ... }: with import <stockholm/lib>;
{
  imports = [ <stockholm/krebs/2configs/syncthing.nix> ];
  services.syncthing = {
    group = "syncthing";
    declarative = {
      key = toString <secrets/syncthing.key>;
      cert = toString <secrets/syncthing.cert>;
    };
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 22000"; target = "ACCEPT";}
    { predicate = "-p udp --dport 21027"; target = "ACCEPT";}
  ];

  system.activationScripts.syncthing-home = mkDefault ''
    ${pkgs.coreutils}/bin/chmod a+x /home/lass
  '';

  boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;
}
