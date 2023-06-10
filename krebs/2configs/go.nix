{ config, lib, pkgs, ... }:

{
  krebs.go = {
    enable = true;
  };
  networking.firewall.allowedTCPPorts = [ 80 ];
  services.nginx = {
    enable = true;
    virtualHosts.go = {
      locations."/".extraConfig = ''
        proxy_set_header Host go.r;
        proxy_pass http://localhost:1337;
      '';
      serverAliases = [
        "go"
        "go.r"
      ];
    };
  };
}
