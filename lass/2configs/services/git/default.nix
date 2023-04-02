{ config, lib, pkgs, ... }:
{
  imports = [
     ../../git.nix
  ];
  services.nginx.virtualHosts."cgit.lassul.us" = {
    enableACME = true;
    addSSL = true;
    locations = config.services.nginx.virtualHosts.cgit.locations;
  };
}
