let
  port = 8086;
in
{
  networking.firewall.allowedTCPPorts = [ port ]; # for legacy applications
  services.nginx.virtualHosts."influx.shack" = {
    locations."/" = {
      proxyPass = "http://localhost:${toString port}/";
    };
  };
  services.influxdb = {
    enable = true;
    extraConfig = {
      bind-address = ":${toString port}";
      http.log-enabled = false;
    };
  };
}
