{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/libvirt.nix>
    { # TODO make new hfos.nix out of this vv
      boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
      users.users.riot = {
        uid = genid_uint31 "riot";
        isNormalUser = true;
        extraGroups = [ "libvirtd" ];
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC6o6sdTu/CX1LW2Ff5bNDqGEAGwAsjf0iIe5DCdC7YikCct+7x4LTXxY+nDlPMeGcOF88X9/qFwdyh+9E4g0nUAZaeL14Uc14QDqDt/aiKjIXXTepxE/i4JD9YbTqStAnA/HYAExU15yqgUdj2dnHu7OZcGxk0ZR1OY18yclXq7Rq0Fd3pN3lPP1T4QHM9w66r83yJdFV9szvu5ral3/QuxQnCNohTkR6LoJ4Ny2RbMPTRtb+jPbTQYTWUWwV69mB8ot5nRTP4MRM9pu7vnoPF4I2S5DvSnx4C5zdKzsb7zmIvD4AmptZLrXj4UXUf00Xf7Js5W100Ne2yhYyhq+35 riot@lagrange"
        ];
      };

      # TODO write function for proxy_pass (ssl/nonssl)

      krebs.iptables.tables.filter.FORWARD.rules = [
        { v6 = false; precedence = 1000; predicate = "-d 192.168.122.179"; target = "ACCEPT"; }
      ];
      krebs.iptables.tables.nat.PREROUTING.rules = [
        { v6 = false; precedence = 1000; predicate = "-d 46.4.114.243"; target = "DNAT --to-destination 192.168.122.179"; }
      ];
    }
    <stockholm/lass/2configs/container-networking.nix>
    {
      services.taskserver = {
        enable = true;
        fqdn = "lassul.us";
        listenHost = "::";
        listenPort = 53589;
        organisations.lass.users = [ "lass" "android" ];
      };
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport 53589"; target = "ACCEPT"; }
      ];
    }
    {
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p udp --dport 60000:61000"; target = "ACCEPT";}
      ];
    }
  ];

  krebs.build.host = config.krebs.hosts.archprism;
  services.earlyoom = {
    enable = true;
    freeMemThreshold = 5;
  };
}
