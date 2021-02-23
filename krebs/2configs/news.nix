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
      serverAliases = [
        "news.r"
      ];
      locations."/".extraConfig = ''
        root /var/lib/brockman;
        index brockman.json;
      '';
      extraConfig = ''
        add_header 'Access-Control-Allow-Origin' '*';
        add_header 'Access-Control-Allow-Methods' 'GET, POST, OPTIONS';
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

  krebs.reaktor2.news = let
    name = "candyman";
  in {
    hostname = "localhost";
    port = "6667";
    nick = name;
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
              pattern = "^${name}:\\s*(\\S*)(?:\\s+(.*\\S))?\\s*$";
              command = 1;
              arguments = [2];
              commands = {
                add-reddit.filename = pkgs.writeDash "add-reddit" ''
                  set -euf
                  if [ "$#" -ne 1 ]; then
                    echo 'usage: ${name}: add-reddit $reddit_channel'
                    exit 1
                  fi
                  reddit_channel=$(echo "$1" | ${pkgs.jq}/bin/jq -Rr '[match("(\\S+)\\s*";"g").captures[].string][0]')
                  echo "brockman: add r_$reddit_channel http://rss.r/?action=display&bridge=Reddit&context=single&r=$reddit_channel&format=Atom"
                '';
                add-telegram.filename = pkgs.writeDash "add-telegram" ''
                  set -euf
                  if [ "$#" -ne 1 ]; then
                    echo 'usage: ${name}: add-telegram $telegram_user'
                    exit 1
                  fi
                  telegram_user=$(echo "$1" | ${pkgs.jq}/bin/jq -Rr '[match("(\\S+)\\s*";"g").captures[].string][0]')
                  echo "brockman: add t_$telegram_user http://rss.r/?action=display&bridge=Telegram&username=$telegram_user&format=Mrss"
                '';
                add-youtube.filename = pkgs.writeDash "add-youtube" ''
                  set -euf
                  if [ "$#" -ne 1 ]; then
                    echo 'usage: ${name}: add-youtube $nick $channelid'
                    exit 1
                  fi
                  youtube_nick=$(echo "$1" | ${pkgs.jq}/bin/jq -Rr '[match("(\\S+)\\s*";"g").captures[].string][0]')
                  youtube_id=$(echo "$1" | ${pkgs.jq}/bin/jq -Rr '[match("(\\S+)\\s*";"g").captures[].string][1]')
                  echo "brockman: add yt_$youtube_nick http://rss.r/?action=display&bridge=Youtube&context=By+channel+id&c=$youtube_id&duration_min=&duration_max=&format=Mrss"
                '';
                search.filename = pkgs.writeDash "search" ''
                  set -euf
                  if [ "$#" -ne 1 ]; then
                    echo 'usage: ${name}: search $searchterm'
                    exit 1
                  fi
                  searchterm=$(echo "$1" | ${pkgs.jq}/bin/jq -Rr '[match("(\\S+)\\s*";"g").captures[].string][0]')
                  ${pkgs.curl}/bin/curl -Ss "https://feedsearch.dev/api/v1/search?url=$searchterm&info=true&favicon=false" |
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
