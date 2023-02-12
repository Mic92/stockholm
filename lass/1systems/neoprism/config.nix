{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/retiolum.nix>

    # sync-containers
    <stockholm/lass/2configs/consul.nix>
    <stockholm/lass/2configs/yellow-host.nix>
    <stockholm/lass/2configs/radio/container-host.nix>
    <stockholm/lass/2configs/ubik-host.nix>
    <stockholm/krebs/2configs/hotdog-host.nix>

    # other containers
    <stockholm/lass/2configs/riot.nix>
  ];

  krebs.build.host = config.krebs.hosts.neoprism;

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  services.nginx.enable = true;
  security.acme.acceptTerms = true;
  security.acme.defaults.email = "acme@lassul.us";
}
