{pkgs, config, ...}:
with import <stockholm/lib>;
{
  services.influxdb.enable = true;

  services.influxdb.extraConfig = {
    meta.hostname = config.krebs.build.host.name;
    # meta.logging-enabled = true;
    http.bind-address = ":8086";
    admin.bind-address = ":8083";
    monitoring = {
      enabled = false;
      # write-interval = "24h";
    };
    collectd = [{
      enabled = true;
      typesdb = "${pkgs.collectd}/share/collectd/types.db";
      database = "collectd_db";
      port = 25826;
    }];
  };

  lass.kapacitor =
    let
      db = "telegraf_db";
      echoToIrc = pkgs.writeDash "echo_irc" ''
        set -euf
        data="$(${pkgs.jq}/bin/jq -r .message)"
        export LOGNAME=prism-alarm
        ${pkgs.irc-announce}/bin/irc-announce \
          ni.r 6667 prism-alarm \#retiolum "$data" >/dev/null
      '';
    in {
      enable = true;
      alarms = {
        cpu = {
          database = db;
          text = ''
            var data = batch
              |query(${"'''"}
                SELECT mean("usage_user") AS mean
                FROM "${db}"."default"."cpu"
              ${"'''"})
              .period(10m)
              .every(1m)
              .groupBy('host')
              data |alert()
                .crit(lambda: "mean" > 90)
                .exec('${echoToIrc}')
              data |deadman(1.0,5m)
                .stateChangesOnly()
                .exec('${echoToIrc}')
          '';
        };
        ram = {
          database = db;
          text = ''
            var data = batch
              |query(${"'''"}
                SELECT mean("used_percent") AS mean
                FROM "${db}"."default"."mem"
              ${"'''"})
              .period(10m)
              .every(1m)
              .groupBy('host')
              data |alert()
                .crit(lambda: "mean" > 90)
                .exec('${echoToIrc}')
          '';
        };
      };
  };

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp -i retiolum --dport 8086"; target = "ACCEPT"; }
    { predicate = "-p tcp -i retiolum --dport 3000"; target = "ACCEPT"; }
    { predicate = "-p udp -i retiolum --dport 25826"; target = "ACCEPT"; }
  ];
  services.grafana = {
    enable = true;
    addr = "0.0.0.0";
    auth.anonymous.enable = true;
    security = import <secrets/grafana_security.nix>; # { AdminUser = ""; adminPassword = ""}
  };
}
