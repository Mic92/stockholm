{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
{
  users.users.makefu = {
    uid = genid "makefu";
    isNormalUser = true;
    extraGroups = [ "libvirtd" ];
    openssh.authorizedKeys.keys = [
      config.krebs.users.makefu.pubkey
    ];
  };

  krebs.iptables.tables.nat.PREROUTING.rules = [
    { v6 = false; precedence = 1000; predicate = "-d 213.239.205.246 -p tcp --dport 10022"; target = "DNAT --to-destination 192.168.122.136:22"; }
  ];

  krebs.iptables.tables.filter.FORWARD.rules = [
    { v6 = false; precedence = 1000; predicate = "-d 192.168.122.136 -p tcp --dport 22 -m state --state NEW,ESTABLISHED,RELATED"; target = "ACCEPT"; }
  ];
}
