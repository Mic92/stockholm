{ config, lib, pkgs, ... }:
{
  containers.riot = {
    config = {
      environment.systemPackages = [
        pkgs.dhcpcd
        pkgs.git
        pkgs.jq
      ];
      networking.useDHCP = lib.mkForce true;
      networking.firewall.enable = false;
      systemd.services.autoswitch = {
        environment = {
          NIX_REMOTE = "daemon";
        };
        wantedBy = [ "multi-user.target" ];
        serviceConfig.ExecStart = pkgs.writers.writeDash "autoswitch" ''
          set -efu
          if test -e /var/src/nixos-config; then
            /run/current-system/sw/bin/nixos-rebuild -I /var/src switch || :
          fi
        '';
        unitConfig.X-StopOnRemoval = false;
      };
    };
    autoStart = true;
    enableTun = true;
    privateNetwork = true;
    hostAddress = "10.233.1.1";
    localAddress = "10.233.1.2";
    forwardPorts = [
      { hostPort = 45622; containerPort = 22; }
    ];
  };

  systemd.network.networks."50-ve-riot" = {
    matchConfig.Name = "ve-riot";

    networkConfig = {
      IPForward = "yes";
      # weirdly we have to use POSTROUTING MASQUERADE here
      # IPMasquerade = "both";
      LinkLocalAddressing = "no";
      KeepConfiguration = "static";
    };
  };

  # networking.nat can be used instead of this
  krebs.iptables.tables.nat.POSTROUTING.rules = [
    { v6 = false; predicate = "-s ${config.containers.riot.localAddress}"; target = "MASQUERADE"; }
  ];
  krebs.iptables.tables.filter.FORWARD.rules = [
    { predicate = "-i ve-riot"; target = "ACCEPT"; }
    { predicate = "-o ve-riot"; target = "ACCEPT"; }
  ];
}
