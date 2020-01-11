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
      disko.urls = [
        "http://cgit.gum.r/disko"
        "http://cgit.hotdog.r/disko"
        "http://cgit.ni.r/disko"
        "http://cgit.prism.r/disko"
      ];
      krops.urls = [
        "http://cgit.hotdog.r/krops"
        "http://cgit.ni.r/krops"
        "http://cgit.prism.r/krops"
        "https://git.ingolf-wagner.de/krebs/krops.git"
        "https://github.com/krebs/krops.git"
      ];
      nix_writers.urls = [
        "http://cgit.hotdog.r/nix-writers"
        "http://cgit.ni.r/nix-writers"
        "http://cgit.prism.r/nix-writers"
        "https://git.ingolf-wagner.de/krebs/nix-writers.git"
      ];
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
