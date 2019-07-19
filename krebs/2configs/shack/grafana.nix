let
  port = 3000;
in {

  networking.firewall.allowedTCPPorts = [ port ]; # legacy
  services.nginx.virtualHosts."grafana.shack" = {
    locations."/".proxyPass = "http://localhost:${toString port}";
  };
  services.grafana = {
    enable = true;
    port = port;
    addr = "0.0.0.0";
    users.allowSignUp = true;
    users.allowOrgCreate = true;
    users.autoAssignOrg = true;
    auth.anonymous.enable = true;
    security = import <secrets/grafana_security.nix>;
  };
}
