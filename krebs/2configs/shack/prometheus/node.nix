{ config, lib, pkgs, ... }:
{
  networking.firewall.allowedTCPPorts = [ 9100 ];

  services.prometheus.exporters.node = {
    enable = true;
    enabledCollectors = [
      "conntrack"
      "diskstats"
      "entropy"
      "filefd"
      "filesystem"
      "loadavg"
      "mdadm"
      "meminfo"
      "netdev"
      "netstat"
      "stat"
      "time"
      "vmstat"
      "systemd"
      "logind"
      "interrupts"
      "ksmd"
    ];
  };
}
