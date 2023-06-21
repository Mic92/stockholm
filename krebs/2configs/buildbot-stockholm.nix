{ config, lib, ... }:
with import ../../lib/pure.nix { inherit lib; };
{
  networking.firewall.allowedTCPPorts = [ 80 ];
  services.nginx = {
    enable = true;
    virtualHosts.build = {
      serverAliases = [ "build.r" "build.${config.networking.hostName}.r" ];
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.buildbot-master.port}";
        proxyWebsockets = true;
        extraConfig = ''
          proxy_read_timeout 3600s;
        '';
      };
    };
  };
  krebs.ci = {
    enable = true;
    repos = {
      disko.urls = [
        "http://cgit.gum.r/disko"
        "http://cgit.ni.r/disko"
        "http://cgit.orange.r/disko"
      ];
      krops.urls = [
        "http://cgit.ni.r/krops"
        "http://cgit.orange.r/krops"
        "https://github.com/krebs/krops.git"
      ];
      nix_writers.urls = [
        "http://cgit.ni.r/nix-writers"
        "http://cgit.orange.r/nix-writers"
      ];
      stockholm.urls = [
        "http://cgit.gum.r/stockholm"
        "http://cgit.ni.r/stockholm"
        "http://cgit.orange.r/stockholm"
      ];
    };
  };
}
