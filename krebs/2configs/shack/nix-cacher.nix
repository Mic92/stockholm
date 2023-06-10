{ config, pkgs, ... }:
with import ../../../lib/pure.nix { inherit lib; };
let
  cfg = config.krebs.apt-cacher-ng;
in
{
  imports = [
    ./bincache.nix
  ];
  krebs.apt-cacher-ng = {
    enable = true;
    port = 3142;
    bindAddress = "localhost";
    cacheExpiration = 30;
  };

  services.nginx = {
    enable = mkDefault true;
    virtualHosts.shack-nix-cacher = {
      serverAliases = [
        "acng.shack"
      ];
      locations."/".extraConfig = ''
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_pass http://localhost:${toString cfg.port}/;
      '';
    };
  };
}
