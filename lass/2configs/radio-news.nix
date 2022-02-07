{ config, pkgs, ... }: with pkgs.stockholm.lib;
let
  weather_report = pkgs.writers.writeDashBin "weather_report" ''
  set -efu
  ${pkgs.curl}/bin/curl -sSL https://wttr.in/''${1-}?format=j1 \
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

  newsshow = pkgs.writers.writeDashBin "newsshow" /* sh */ ''
    echo "
    hello crabpeople!
    $(${pkgs.ddate}/bin/ddate | sed 's/YOLD/Year of Discord/')!
    It is $(date --utc +%H) o clock UTC.
    $(weather_report berlin)
    $(weather_report stuttgart)
    $(weather_report munich)
    "
  '';
in
{
  systemd.services.newsshow = {
    path = [
      newsshow
      send_to_radio
      weather_report
      pkgs.curl
    ];
    script = ''
      set -efu
      newsshow |
        curl -SsG http://tts.r/api/tts --data-urlencode 'text@-' |
        send_to_radio
    '';
    startAt = "*:00:00";
  };

  ## debug
  # environment.systemPackages = [
  #   weather_report
  #   send_to_radio
  #   newsshow
  # ];
}
