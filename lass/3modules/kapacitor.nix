{ config, lib, pkgs, ... }:

with builtins;
with lib;

let
  cfg = config.lass.kapacitor;

  out = {
    options.lass.kapacitor = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "kapacitor";
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/kapacitor";
    };
    user = mkOption {
      type = types.str;
      default = "kapacitor";
    };
    config = mkOption {
      type = types.str;
      #TODO: find a good default
      default = ''
        hostname = "localhost"
        data_dir = "${cfg.dataDir}"

        [http]
          bind-address = ":9092"
          auth-enabled = false
          log-enabled = true
          write-tracing = false
          pprof-enabled = false
          https-enabled = false
          https-certificate = "/etc/ssl/kapacitor.pem"
          shutdown-timeout = "10s"
          shared-secret = ""

        [replay]
          dir = "${cfg.dataDir}/replay"

        [storage]
          boltdb = "${cfg.dataDir}/kapacitor.db"

        [task]
          dir = "${cfg.dataDir}/tasks"
          snapshot-interval = "1m0s"

        [[influxdb]]
          enabled = true
          name = "default"
          default = false
          urls = ["http://localhost:8086"]
          username = ""
          password = ""
          ssl-ca = ""
          ssl-cert = ""
          ssl-key = ""
          insecure-skip-verify = false
          timeout = "0s"
          disable-subscriptions = false
          subscription-protocol = "http"
          udp-bind = ""
          udp-buffer = 1000
          udp-read-buffer = 0
          startup-timeout = "5m0s"
          subscriptions-sync-interval = "1m0s"
          [influxdb.subscriptions]
          [influxdb.excluded-subscriptions]
            _kapacitor = ["autogen"]

        [logging]
          file = "STDERR"
          level = "INFO"

        [collectd]
          enabled = false
          bind-address = ":25826"
          database = "collectd"
          retention-policy = ""
          batch-size = 5000
          batch-pending = 10
          batch-timeout = "10s"
          read-buffer = 0
          typesdb = "/usr/share/collectd/types.db"

        [opentsdb]
          enabled = false
          bind-address = ":4242"
          database = "opentsdb"
          retention-policy = ""
          consistency-level = "one"
          tls-enabled = false
          certificate = "/etc/ssl/influxdb.pem"
          batch-size = 1000
          batch-pending = 5
          batch-timeout = "1s"
          log-point-errors = true

        [smtp]
          enabled = false
          host = "localhost"
          port = 25
          username = ""
          password = ""
          no-verify = false
          global = false
          state-changes-only = false
          from = ""
          idle-timeout = "30s"

        [opsgenie]
          enabled = false
          api-key = ""
          url = "https://api.opsgenie.com/v1/json/alert"
          recovery_url = "https://api.opsgenie.com/v1/json/alert/note"
          global = false

        [victorops]
          enabled = false
          api-key = ""
          routing-key = ""
          url = "https://alert.victorops.com/integrations/generic/20131114/alert"
          global = false

        [pagerduty]
          enabled = false
          url = "https://events.pagerduty.com/generic/2010-04-15/create_event.json"
          service-key = ""
          global = false

        [sensu]
          enabled = false
          addr = ""
          source = "Kapacitor"

        [slack]
          enabled = false
          url = ""
          channel = ""
          global = false
          state-changes-only = false

        [telegram]
          enabled = false
          url = "https://api.telegram.org/bot"
          token = ""
          chat-id = ""
          parse-mode = ""
          disable-web-page-preview = false
          disable-notification = false
          global = false
          state-changes-only = false

        [hipchat]
          enabled = false
          url = ""
          token = ""
          room = ""
          global = false
          state-changes-only = false

        [alerta]
          enabled = false
          url = ""
          token = ""
          environment = ""
          origin = ""

        [reporting]
          enabled = true
          url = "https://usage.influxdata.com"

        [stats]
          enabled = true
          stats-interval = "10s"
          database = "_kapacitor"
          retention-policy = "autogen"
          timing-sample-rate = 0.1
          timing-movavg-size = 1000

        [udf]

        [deadman]
          interval = "10s"
          threshold = 0.0
          id = "{{ .Group }}:NODE_NAME for task '{{ .TaskName }}'"
          message = "{{ .ID }} is {{ if eq .Level \"OK\" }}alive{{ else }}dead{{ end }}: {{ index .Fields \"emitted\" | printf \"%0.3f\" }} points/INTERVAL."
          global = false

        [talk]
          enabled = false
          url = ""
          author_name = ""
      '';
      description = "configuration kapacitor is started with";
    };
  };

  configFile = pkgs.writeText "kapacitor.conf" cfg.config;

  imp = {

    systemd.services.kapacitor = {
      description = "kapacitor";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      restartIfChanged = true;

      serviceConfig = {
        Restart = "always";
        ExecStart = "${pkgs.kapacitor}/bin/kapacitord -config ${configFile}";
      };
    };
  };

in out
