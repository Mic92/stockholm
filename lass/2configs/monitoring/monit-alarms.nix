{pkgs, config, ...}:
with import <stockholm/lib>;
let
  echoToIrc = msg:
    pkgs.writeDash "echo_irc" ''
      set -euf
      export LOGNAME=prism-alarm
      ${pkgs.irc-announce}/bin/irc-announce \
        ni.r 6667 prism-alarm \#retiolum "${msg}" >/dev/null
    '';

in {
  krebs.monit = {
    enable = true;
    http.enable = true;
    alarms.nirwanabluete = {
      test = "${pkgs.curl}/bin/curl -sf 'https://nirwanabluete.de/'";
      alarm = echoToIrc "test nirwanabluete failed";
    };
    alarms.ubik = {
      test = "${pkgs.curl}/bin/curl -sf 'https://ubikmedia.de'";
      alarm = echoToIrc "test ubik failed";
    };
    alarms.hfos = {
      test = "${pkgs.curl}/bin/curl -sf --insecure 'https://hfos.hackerfleet.de'";
      alarm = echoToIrc "test hfos failed";
    };
    alarms.cac-panel = {
      test = "${pkgs.curl}/bin/curl -sf 'https://panel.cloudatcost.com/login.php'";
      alarm = echoToIrc "test cac-panel failed";
    };
  };
}

