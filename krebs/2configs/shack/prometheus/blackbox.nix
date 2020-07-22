{pkgs, ... }:
{
  systemd.services.prometheus-blackbox-exporter.serviceConfig = {
    CapabilityBoundingSet = ["CAP_NET_RAW"]; # icmp allow
    AmbientCapabilities = ["CAP_NET_RAW"];
  };
  services.prometheus.exporters.blackbox = {
    enable = true;
    # openFirewall = true; # not requred if running on the same host as prometheus
    port = 9115;
    configFile = pkgs.writeText "icmp" ''
      modules:
        icmp:
          prober: icmp
          icmp:
            preferred_ip_protocol: ip4
    '';
  };
}
