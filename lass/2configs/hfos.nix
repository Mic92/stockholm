{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
{
  users.users.riot = {
    uid = genid "riot";
    isNormalUser = true;
    extraGroups = [ "libvirtd" ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC5NnADMRySix1kcxQwseHfem/SCDmkbvwc+ZZu7HFz4zss1k4Fh1knsukMY83zlno8p/8bBPWyixLTxuZHNy26af8GP95bvV3brnpRmrijkE4dOlpd+wvPcIyTKNunJvMzNDP/ry9g2GczEZKGWvQZudq/nI54HaCaRWM2kzEMEg8Rr9SGlZEKo8B+8HGVsz1a8USOnm8dqYP9dmfLdpy/s+7yWJSPh8wokvWeOOrahirOhO99ZfXm2gcdHqSKvbD2+4EYEm5w8iFrbYBT2wZ3u9ZOiooL/JuEBBdnDrcqZqeaTw0vOdKPvkUP8/rzRjvIwSkynMSD8fixpdGRNeIB riot@lagrange"
      config.krebs.users.lass.pubkey
    ];
  };

  networking.interfaces.et0.ip4 = [
    {
      address = "213.239.205.246";
      prefixLength = 24;
    }
  ];

  krebs.iptables.tables.nat.PREROUTING.rules = [
    { v6 = false; precedence = 1000; predicate = "-d 213.239.205.246 -p tcp --dport 22"; target = "DNAT --to-destination 192.168.122.208:22"; }
    { v6 = false; precedence = 1000; predicate = "-d 213.239.205.246 -p tcp --dport 80"; target = "DNAT --to-destination 192.168.122.208:80"; }
    { v6 = false; precedence = 1000; predicate = "-d 213.239.205.246 -p tcp --dport 443"; target = "DNAT --to-destination 192.168.122.208:1443"; }
  ];

  krebs.iptables.tables.filter.FORWARD.rules = [
    { v6 = false; precedence = 1000; predicate = "-d 192.168.122.208 -p tcp --dport 22 -m state --state NEW,ESTABLISHED,RELATED"; target = "ACCEPT"; }
    { v6 = false; precedence = 1000; predicate = "-d 192.168.122.208 -p tcp --dport 80 -m state --state NEW,ESTABLISHED,RELATED"; target = "ACCEPT"; }
    { v6 = false; precedence = 1000; predicate = "-d 192.168.122.208 -p tcp --dport 1443 -m state --state NEW,ESTABLISHED,RELATED"; target = "ACCEPT"; }
  ];
}
