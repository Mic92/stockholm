{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/git.nix>
    {
      networking.dhcpcd.allowInterfaces = [
        "enp*"
        "eth*"
        "ens*"
      ];
    }
    {
      sound.enable = false;
    }
    {
      environment.systemPackages = with pkgs; [
        mk_sql_pair
      ];
    }
    {
      imports = [
        <stockholm/lass/2configs/websites/fritz.nix>
      ];
      krebs.iptables.tables.filter.INPUT.rules = [
         { predicate = "-p tcp --dport http"; target = "ACCEPT"; }
         { predicate = "-p tcp --dport https"; target = "ACCEPT"; }
      ];
    }
    {
      #TODO: abstract & move to own file
      krebs.exim-smarthost = {
        enable = true;
        relay_from_hosts = map (host: host.nets.retiolum.ip4.addr) [
          config.krebs.hosts.mors
          config.krebs.hosts.uriel
        ];
        system-aliases = [
          { from = "mailer-daemon"; to = "postmaster"; }
          { from = "postmaster"; to = "root"; }
          { from = "nobody"; to = "root"; }
          { from = "hostmaster"; to = "root"; }
          { from = "usenet"; to = "root"; }
          { from = "news"; to = "root"; }
          { from = "webmaster"; to = "root"; }
          { from = "www"; to = "root"; }
          { from = "ftp"; to = "root"; }
          { from = "abuse"; to = "root"; }
          { from = "noc"; to = "root"; }
          { from = "security"; to = "root"; }
          { from = "root"; to = "lass"; }
        ];
      };
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport smtp"; target = "ACCEPT"; }
      ];
    }
  ];

  krebs.build.host = config.krebs.hosts.dishfire;
}
