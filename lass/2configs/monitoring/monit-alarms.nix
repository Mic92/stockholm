{pkgs, config, ...}:
with import <stockholm/lib>;
let
  echoToIrc = msg:
    pkgs.writeDash "echo_irc" ''
      set -euf
      export LOGNAME=prism-alarm
      ${pkgs.irc-announce}/bin/irc-announce \
        irc.r 6667 ${config.networking.hostName}-alarm \#noise "${msg}" >/dev/null
    '';

in {
  krebs.monit = {
    enable = true;
    http.enable = true;
    alarms = {
      nirwanabluete = {
        test = "${pkgs.curl}/bin/curl -sf 'https://nirwanabluete.de/'";
        alarm = echoToIrc "test nirwanabluete failed";
      };
      ubik = {
        test = "${pkgs.curl}/bin/curl -sf 'https://ubikmedia.de'";
        alarm = echoToIrc "test ubik failed";
      };
      cac-panel = {
        test = "${pkgs.curl}/bin/curl -sf 'https://panel.cloudatcost.com/login.php'";
        alarm = echoToIrc "test cac-panel failed";
      };
      radio = {
        test = pkgs.writeBash "check_stream" ''
          ${pkgs.curl}/bin/curl -sif http://lassul.us:8000/radio.ogg \
          | ${pkgs.gawk}/bin/awk '/^\r$/{exit}{print $0}' \
          | ${pkgs.gnugrep}/bin/grep -q "200 OK" || exit "''${PIPESTATUS[0]}"
        '';
        alarm = echoToIrc "test radio failed";
      };
    };
  };

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp -i retiolum --dport 9093"; target = "ACCEPT"; }
  ];
}

