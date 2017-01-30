{pkgs, config, ...}:
with import <stockholm/lib>;
{
  services.influxdb = {
    enable = true;
  };

  services.influxdb.extraConfig = {
    meta.hostname = config.krebs.build.host.name;
    # meta.logging-enabled = true;
    http.bind-address = ":8086";
    admin.bind-address = ":8083";
    monitoring = {
      enabled = false;
      # write-interval = "24h";
    };
  };

  lass.kapacitor =
    let
      echoToIrc = pkgs.writeDash "echo_irc" ''
        set -euf
        data="$(${pkgs.jq}/bin/jq -r .message)"
        export LOGNAME=prism-alarm
        ${pkgs.irc-announce}/bin/irc-announce \
          irc.freenode.org 6667 prism-alarm \#krebs-bots "$data" >/dev/null
      '';
    in {
      enable = true;
      alarms = {
        test2 = ''
          batch
            |query(${"'''"}
              SELECT mean("usage_user") AS mean
              FROM "${config.lass.kapacitor.check_db}"."default"."cpu"
            ${"'''"})
            .every(3m)
            .period(1m)
            .groupBy('host')
            |alert()
              .crit(lambda: "mean" >  90)
              // Whenever we get an alert write it to a file.
              .log('/tmp/alerts.log')
              .exec('${echoToIrc}')
        '';
      };
  };

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp -i retiolum --dport 8086"; target = "ACCEPT"; }
    { predicate = "-p tcp -i retiolum --dport 3000"; target = "ACCEPT"; }
  ];
  services.grafana = {
    enable = true;
    addr = "0.0.0.0";
    auth.anonymous.enable = true;
    security = import <secrets/grafana_security.nix>; # { AdminUser = ""; adminPassword = ""}
  };
}
