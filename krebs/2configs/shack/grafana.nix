let
  port = 3000;
in {

  networking.firewall.allowedTCPPorts = [ port ]; # legacy
  services.nginx.virtualHosts."grafana.shack" = {
    locations."/" = {
      proxyPass = "http://localhost:${toString port}";
      extraConfig =''
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header Host             $host;
          proxy_set_header X-Real-IP        $remote_addr;
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection "upgrade";
      '';

    };
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
