{pkgs, ... }: # hostname: influx.shack
let
  port = 8086;
  collectd-port = 25826;
  db = "collectd_db";
in
{
  networking.firewall.allowedTCPPorts = [ port ]; # for legacy applications
  networking.firewall.allowedUDPPorts = [ collectd-port ];
  services.nginx.virtualHosts."influx.shack" = {
    locations."/" = {
      proxyPass = "http://localhost:${toString port}/";
    };
  };
  services.influxdb = {
    enable = true;
    extraConfig = {
      http.bind-address = "0.0.0.0:${toString port}";
      http.log-enabled = false;
      http.write-tracing = false;
      http.suppress-write-log = true;
      data.trace-logging-enabled = false;
      data.query-log-enabled = false;
      monitoring.enabled = false;
      collectd = [{
        enabled = true;
        typesdb = "${pkgs.collectd}/share/collectd/types.db";
        database = db;
        bind-address = ":${toString collectd-port}";
      }];
    };
  };
}
