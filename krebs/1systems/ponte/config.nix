{ config, pkgs, ... }:
{
  imports = [
    ./hw.nix
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    <stockholm/krebs/2configs/matterbridge.nix>
  ];

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  krebs.build.host = config.krebs.hosts.ponte;

  krebs.pages.enable = true;
  krebs.pages.nginx.addSSL = true;
  krebs.pages.nginx.enableACME = true;

  security.acme.acceptTerms = true;
  security.acme.certs.${config.krebs.pages.domain}.email = "spam@krebsco.de";
}
