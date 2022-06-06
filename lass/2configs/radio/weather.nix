{ config, lib, pkgs, ... }:
let
  weather_for_ips = pkgs.writers.writePython3Bin "weather_for_ips" {
    libraries = [ pkgs.python3Packages.geoip2 ];
    flakeIgnore = [ "E501" ];
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
in {
  systemd.services.weather = {
    path = [
      weather_report
      pkgs.retry
      pkgs.jq
      pkgs.curl
    ];
    script = ''
      set -xefu
      retry -t 5 -d 10 -- weather_report |
        jq \
          --arg from "$(date -u +'%FT%TZ')" \
          --arg to "$(date -u +'%FT%TZ' -d '+1 hours')" \
          --slurp --raw-input --compact-output --ascii-output \
          '{text: ., from: $from, to: $to, priority: 100}' |
        retry -t 5 -d 10 -- curl -v -d@- http://radio-news.r
    '';
    startAt = "*:58:00";
    serviceConfig = {
      User = "radio-news";
      LoadCredential = [
        "openweather_api:${toString <secrets>}/openweather_api_key"
      ];
    };
  };
}
