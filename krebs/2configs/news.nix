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
      locations."/api".extraConfig = ''
        proxy_pass http://127.0.0.1:7777/;
        proxy_pass_header Server;
      '';
      locations."= /graph.html".extraConfig = ''
        alias ${pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/kmein/brockman/05d33c8caaaf6255752f9600981974bb58390851/tools/graph.html";
          sha256 = "0iw2vdzj6kzkix1c447ybmc953lns6z4ap6sr9pcib8bany4g43w";
        }};
      '';
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
    "d /run/irc-api 1750 brockman nginx -"
  ];

  systemd.services.brockman-graph = {
    path = [
      pkgs.graphviz
      pkgs.jq
      pkgs.inotifyTools
    ];
    serviceConfig = {
      ExecStart = pkgs.writers.writeDash "brockman-graph" ''

        while :; do
          graphviz="$(cat /var/lib/brockman/brockman.json \
            | jq -r '
              .bots |
              to_entries |
              map(select(.value.extraChannels|length > 1 )) |
              .[] |
              "\"\(.key)\" -> {\(.value.extraChannels|map("\""+.+"\"")|join(" "))}"
          ')"
          echo "digraph news { $graphviz }" | circo -Tsvg > /var/lib/brockman/graph.svg

          inotifywait -q -e MODIFY /var/lib/brockman/brockman.json
        done
      '';
      User = "brockman";
    };
    wantedBy = [ "multi-user.target" ];
  };

  krebs.ergo.openFilesLimit = 16384;
  krebs.ergo.config = {
    limits.nicklen = 100;
    limits.identlen = 100;
    history.enabled = false;
  };
  systemd.services.brockman.bindsTo = [ "ergo.service" ];
  systemd.services.brockman.serviceConfig.LimitNOFILE = 16384;
  systemd.services.brockman.environment.BROCKMAN_LOG_LEVEL = "DEBUG";
  krebs.brockman = {
    enable = true;
    config = {
      irc.host = "localhost";
      channel = "#all";
      shortener = "http://go.r";
      controller = {
        nick = "brockman";
        extraChannels = [ "#all" ];
      };
      bots = {};
    };
  };

  krebs.reaktor2.api = {
    hostname = "localhost";
    port = "6667";
    nick = "api";
    API.listen = "inet://127.0.0.1:7777";
    plugins = [
      {
        plugin = "register";
        config = {
          channels = [
            "#all"
          ];
        };
      }
    ];
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
            "#lasstube"
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
                    echo 'usage: ${name}: add-youtube $nick $channel/video/stream/id'
                    exit 1
                  fi
                  youtube_nick=$(echo "$1" | ${pkgs.jq}/bin/jq -Rr '[match("(\\S+)\\s*";"g").captures[].string][0]')
                  youtube_url=$(echo "$1" | ${pkgs.jq}/bin/jq -Rr '[match("(\\S+)\\s*";"g").captures[].string][1]')
                  if [ ''${#youtube_url} -eq 24 ]; then
                    youtube_id=$youtube_url
                  else
                    youtube_id=$(${pkgs.youtube-dl}/bin/youtube-dl --max-downloads 1 -j "$youtube_url" | ${pkgs.jq}/bin/jq -r '.channel_id')
                  fi
                  echo "brockman: add yt_$youtube_nick http://rss.r/?action=display&bridge=Youtube&context=By+channel+id&c=$youtube_id&duration_min=&duration_max=&format=Mrss"
                '';
                add-twitch.filename = pkgs.writeDash "add-twitch" ''
                  set -euf
                  if [ "$#" -ne 1 ]; then
                    echo 'usage: ${name}: add-twitch $handle'
                    exit 1
                  fi
                  twitch_nick=$(echo "$1" | ${pkgs.jq}/bin/jq -Rr '[match("(\\S+)\\s*";"g").captures[].string][0]')
                  echo "brockman: add twitch_$twitch_nick http://rss.r/?action=display&bridge=Twitch&channel=$twitch_nick&type=all&format=Atom"
                '';
                add-twitter.filename = pkgs.writeDash "add-twitter" ''
                  set -euf
                  if [ "$#" -ne 1 ]; then
                    echo 'usage: ${name}: add-twitter $handle'
                    exit 1
                  fi
                  twitter_nick=$(echo "$1" | ${pkgs.jq}/bin/jq -Rr '[match("(\\S+)\\s*";"g").captures[].string][0]')
                  echo "brockman: add tw_$twitter_nick http://rss.r/?action=display&bridge=Twitter&context=By+username&u=$twitter_nick&norep=on&noretweet=on&nopinned=on&nopic=on&format=Atom"
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
