{ config, lib, pkgs, ... }:

with lib;
{
  environment.systemPackages = [
    pkgs.go
  ];
  krebs.go = {
    enable = true;
  };
  krebs.nginx = {
    enable = true;
    servers.go = {
      locations = [
        (nameValuePair "/" ''
          proxy_set_header Host go;
          proxy_pass http://localhost:1337;
        '')
      ];
      server-names = [
        "go"
        "go.retiolum"
      ];
    };
  };
}
