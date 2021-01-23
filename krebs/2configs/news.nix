{ config, pkgs, ... }:

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

  krebs.reaktor2.news = {
    hostname = "localhost";
    port = "6667";
    nick = "brockman-helper";
    plugins = [
      {
        plugin = "register";
        config = {
          channels = [
            "#all"
            "#aluhut"
            "#news"
          ];
        };
      }
      {
        plugin = "system";
        config = {
          hooks.PRIVMSG = [
            {
              activate = "match";
              pattern = "^(?:.*\\s)?\\s*brockman-helper:\\s*([0-9A-Za-z._][0-9A-Za-z._-]*)(?:\\s+(.*\\S))?\\s*$";
              command = 1;
              arguments = [2];
              commands = {
                add-telegram.filename = pkgs.writeDash "add-telegram" ''
                  if [ "$#" -ne 1 ]; then
                    echo 'usage: brockman-helper: add-telegram $telegramname'
                    echo "$#"
                    exit 1
                  fi
                  echo "brockman: add t_$1 http://rss.r/?action=display&bridge=Telegram&username=$1&format=Mrss"
                '';
                search.filename = pkgs.writeDash "search" ''
                  if [ "$#" -ne 1 ]; then
                    echo 'usage: brockman-helper: search $searchterm'
                    echo "$#"
                    exit 1
                  fi
                  ${pkgs.curl}/bin/curl -Ss "https://feedsearch.dev/api/v1/search?url=$1&info=true&favicon=false" |
                    ${pkgs.jq}/bin/jq '.[].url'
                '';
              };
            }
          ];
        };
      }
    ];
  };
}
