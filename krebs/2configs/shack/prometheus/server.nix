{ pkgs, lib, config, ... }:
# from https://gist.github.com/globin/02496fd10a96a36f092a8e7ea0e6c7dd
{
  networking = {
    firewall.allowedTCPPorts = [
      9090  # prometheus
      9093  # alertmanager
    ];
  };
  services = {
    nginx.virtualHosts = {
      "prometheus.shack" = {
        locations."/".proxyPass = "http://localhost:9090";
      };
      "alert.prometheus.shack" = {
        locations."/".proxyPass = "http://localhost:9093";
      };
    };
    prometheus = {
      enable = true;
      extraFlags = [
        "-storage.local.retention 720h"
        "-storage.local.series-file-shrink-ratio 0.3"
        "-storage.local.memory-chunks 2097152"
        "-storage.local.max-chunks-to-persist 1048576"
        "-storage.local.index-cache-size.fingerprint-to-metric 2097152"
        "-storage.local.index-cache-size.fingerprint-to-timerange 1048576"
        "-storage.local.index-cache-size.label-name-to-label-values 2097152"
        "-storage.local.index-cache-size.label-pair-to-fingerprints 41943040"
      ];
      ruleFiles = lib.singleton (pkgs.writeText "prometheus-rules.yml" (builtins.toJSON {
            groups = lib.singleton {
              name = "mf-alerting-rules";
              rules = import ./alert-rules.nix { inherit lib; };
            };
          }));
      scrapeConfigs = [
        {
          job_name = "node";
          scrape_interval = "10s";
          static_configs = [
            {
              targets = [
                "localhost:9100"
              ];
              labels = {
                alias = "wolf.shack";
              };
            }
            {
              targets = [
                "localhost:9130"
              ];
              labels = {
                alias = "unifi.shack";
              };
            }
            {
              targets = [
                "10.42.22.184:9100" # puyak.shack
              ];
              labels = {
                alias = "puyak.shack";
              };
            }
            {
              targets = [
                "phenylbutazon.shack:9100"
              ];
              labels = {
                alias = "phenylbutazon.shack";
              };
            }
            {
              targets = [
                "ibuprofen.shack:9100"
              ];
              labels = {
                alias = "ibuprofen.shack";
              };
            }
          ];
        }
      ];
      alertmanagers = [
        { scheme = "http";
          path_prefix = "/";
          static_configs = [ { targets = [ "localhost:9093" ]; } ];
        }
      ];
      alertmanager = {
        enable = true;
        listenAddress = "0.0.0.0";
        configuration = {
          "global" = {
            "smtp_smarthost" = "smtp.example.com:587";
            "smtp_from" = "alertmanager@example.com";
          };
          "route" = {
            "group_by" = [ "alertname" "alias" ];
            "group_wait" = "30s";
            "group_interval" = "2m";
            "repeat_interval" = "4h";
            "receiver" = "team-admins";
          };
          "receivers" = [
            {
              "name" = "team-admins";
              "email_configs" = [
                {
                  "to" = "devnull@example.com";
                  "send_resolved" = true;
                }
              ];
              "webhook_configs" = [
                {
                  "url" = "https://example.com/prometheus-alerts";
                  "send_resolved" = true;
                }
              ];
            }
          ];
        };
      };
    };
  };
}
