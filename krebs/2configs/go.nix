{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
{
  environment.systemPackages = [
    pkgs.go-shortener
  ];
  krebs.go = {
    enable = true;
  };
  services.nginx = {
    enable = true;
    virtualHosts.go = {
      locations."/".extraConfig = ''
        proxy_set_header Host go;
        proxy_pass http://localhost:1337;
      '';
      serverAliases = [
        "go"
        "go.r"
      ];
    };
  };
}
