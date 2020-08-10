{ config, ... }:

{
  services.gollum = {
    enable = true;
  };
  networking.firewall.allowedTCPPorts = [ 80 ];
  services.nginx = {
    enable = true;
    virtualHosts.wiki = {
      serverAliases = [ "wiki.r" "wiki.${config.networking.hostName}.r" ];
      locations."/".extraConfig = ''
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_pass http://127.0.0.1:${toString config.services.gollum.port};
      '';
    };
  };
}
