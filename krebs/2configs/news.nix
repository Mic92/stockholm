{ pkgs, ... }:

{
  services.rss-bridge = {
    enable = true;
    whitelist = [ "*" ];
  };
  services.nginx.virtualHosts = {
    rss-bridge = {
      serverAliases = [
        "rss.r"
      ];
    };
    "brockman.r" = {
      locations."/".extraConfig = ''
        root /var/lib/brockman;
        index brockman.json;
      '';
    };
  };
  systemd.tmpfiles.rules = [
    "d /var/lib/brockman 1750 brockman nginx -"
  ];

  systemd.services.brockman.environment.BROCKMAN_LOG_LEVEL = "DEBUG";
  krebs.brockman = {
    enable = true;
    config = {
      irc.host = "localhost";
      channel = "#all";
      shortener = "http://go.r";
      controller = {
        nick = "brockman";
        channels = [ "#all" ];
      };
      bots = {};
    };
  };
}
