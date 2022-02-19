{ config, lib, pkgs, ... }:
let
  weather_report = pkgs.writers.writeDashBin "weather_report" ''
  set -efu
  ${pkgs.curl}/bin/curl -fsSL https://wttr.in/''${1-}?format=j1 \
    | ${pkgs.jq}/bin/jq -r '
    [.nearest_area[0] | "Weather report for \(.areaName[0].value), \(.country[0].value)."]
    + [.current_condition[0] | "Currently it is \(.weatherDesc[0].value) outside with a temperature of \(.temp_C) degrees."]
    | join("\n")
    '
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
    echo "
    hello crabpeople!
    $(${pkgs.ddate}/bin/ddate | sed 's/YOLD/Year of Discord/')!
    It is $(date --utc +%H) o clock UTC.
    todays news:
    $(get_current_news)
    $(gc_news)
    $(weather_report berlin)
    $(weather_report 70173)
    $(weather_report munich)
    "
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
    };
  };

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 7999"; target = "ACCEPT"; }
  ];

  krebs.htgen.news = {
    port = 7999;
    user = {
      name = "radio-news";
    };
    script = ''. ${pkgs.writers.writeDash "htgen-news" ''
      set -xefu
      case "$Method $Request_URI" in
        "POST /")
          payload=$(head -c "$req_content_length" \
            | sed 's/+/ /g;s/%\(..\)/\\x\1/g;' \
            | xargs -0 echo -e \
          )
          echo "$payload" | jq 'has("from") and has("to") and has("text")' >&2
          echo "$payload" | jq -c '{ from: (.from | fromdate | todate), to: (.to | fromdate | todate), text: .text }' >> $HOME/news
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
