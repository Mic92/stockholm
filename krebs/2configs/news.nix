{ pkgs, ... }:

{
  services.rss-bridge = {
    enable = true;
    whitelist = [ "*" ];
  };
  services.nginx.virtualHosts.rss-bridge = {
    serverAliases = [
      "rss.r"
    ];
  };

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
