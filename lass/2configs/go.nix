{ config, lib, pkgs, ... }:

with lib;
{
  imports = [
    ../3modules/go.nix
  ];
  environment.systemPackages = [
    pkgs.go
  ];
  lass.go = {
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
