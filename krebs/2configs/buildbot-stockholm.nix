{ config, ... }: with import <stockholm/lib>;

{
  networking.firewall.allowedTCPPorts = [ 80 ];
  services.nginx = {
    enable = true;
    virtualHosts.build = {
      serverAliases = [ "build.${config.networking.hostName}.r" ];
      locations."/".extraConfig = ''
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_pass http://127.0.0.1:${toString config.krebs.buildbot.master.web.port};
      '';
    };
  };
  krebs.ci = {
    enable = true;
    repos = {
      stockholm.urls = [
        "http://cgit.enklave.r/stockholm"
        "http://cgit.gum.r/stockholm"
        "http://cgit.hotdog.r/stockholm"
        "http://cgit.ni.r/stockholm"
        "http://cgit.prism.r/stockholm"
      ];
    };
  };
}
