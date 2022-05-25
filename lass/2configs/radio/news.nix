{ config, lib, pkgs, ... }:
let
  weather_for_ips = pkgs.writers.writePython3Bin "weather_for_ips" {
    libraries = [ pkgs.python3Packages.geoip2 ];
  } ./weather_for_ips.py;

  weather_report = pkgs.writers.writeDashBin "weather_report" ''
    set -efu
    export PATH="${lib.makeBinPath [
      pkgs.coreutils
      pkgs.curl
      pkgs.iproute2
      pkgs.jc
      pkgs.jq
    ]}"
    curl -z /tmp/GeoLite2-City.mmdb -o /tmp/GeoLite2-City.mmdb http://c.r/GeoLite2-City.mmdb
    MAXMIND_GEOIP_DB="/tmp/GeoLite2-City.mmdb"; export MAXMIND_GEOIP_DB
    OPENWEATHER_API_KEY=$(cat "$CREDENTIALS_DIRECTORY/openweather_api"); export OPENWEATHER_API_KEY
    ss -no 'sport = :8000' |
      jc --ss | jq -r '.[] |
        select(
          .local_address != "[::ffff:127.0.0.1]"
          and .local_address != "[::1]"
        ) | .peer_address | gsub("[\\[\\]]"; "")
      ' |
      ${weather_for_ips}/bin/weather_for_ips
  '';

  send_to_radio = pkgs.writers.writeDashBin "send_to_radio" ''
    ${pkgs.vorbisTools}/bin/oggenc - |
      ${pkgs.libshout}/bin/shout --format ogg --host localhost --port 1338 --mount /live
  '';

  gc_news = pkgs.writers.writeDashBin "gc_news" ''
    set -xefu
    ${pkgs.coreutils}/bin/cat $HOME/news | ${pkgs.jq}/bin/jq -cs 'map(select((.to|fromdateiso8601) > now)) | .[]' > $HOME/bla-news.tmp
    ${pkgs.coreutils}/bin/mv $HOME/bla-news.tmp $HOME/news
  '';

  get_current_news = pkgs.writers.writeDashBin "get_current_news" ''
    set -xefu
    ${pkgs.coreutils}/bin/cat $HOME/news | ${pkgs.jq}/bin/jq -rs 'map(select(((.to | fromdateiso8601) > now) and (.from|fromdateiso8601) < now) | .text) | .[]'
  '';

  newsshow = pkgs.writers.writeDashBin "newsshow" /* sh */ ''
    cat << EOF
    hello crabpeople!
    $(${pkgs.ddate}/bin/ddate +'Today is %{%A, the %e of %B%}, %Y. %N%nCelebrate %H')
    It is $(date --utc +%H) o clock UTC.
    todays news:
    $(get_current_news)
    $(gc_news)
    $(weather_report)
    EOF
  '';
in
{
  systemd.services.newsshow = {
    path = [
      newsshow
      send_to_radio
      gc_news
      get_current_news
      weather_report
      pkgs.curl
      pkgs.retry
    ];
    script = ''
      set -efu
      retry -t 5 -d 10 -- newsshow |
        retry -t 5 -d 10 -- curl -fSsG http://tts.r/api/tts --data-urlencode 'text@-' |
        retry -t 5 -d 10 -- send_to_radio
    '';
    startAt = "*:00:00";
    serviceConfig = {
      User = "radio-news";
      LoadCredential = [
        "openweather_api:${toString <secrets>}/openweather_api_key"
      ];
    };
  };

  services.nginx.virtualHosts."radio-news.r" = {
    locations."/" = {
      proxyPass = "http://localhost:7999";
      proxyWebsockets = true;
      extraConfig = ''
        add_header 'Access-Control-Allow-Origin' '*';
        add_header 'Access-Control-Allow-Methods' 'GET, POST, OPTIONS';
      '';
    };
  };
  krebs.htgen.news = {
    port = 7999;
    user = {
      name = "radio-news";
    };
    script = ''. ${pkgs.writers.writeDash "htgen-news" ''
      set -xefu
      case "''${Method:-GET} $Request_URI" in
        "GET /")
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'
          cat "$HOME"/news | jq -sc .
          exit
        ;;
        "POST /")
          payload=$(head -c "$req_content_length")
          echo "$payload" | jq 'has("from") and has("to") and has("text")' >&2
          echo "$payload" | jq -c '{ from: (.from | fromdate | todate), to: (.to | fromdate | todate), text: .text }' >> "$HOME"/news
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'
          exit
        ;;
      esac
    ''}'';
  };

  ## debug
  # environment.systemPackages = [
  #   weather_report
  #   send_to_radio
  #   newsshow
  # ];
}
