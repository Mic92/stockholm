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
        "-storage.local.retention 8760h"
        "-storage.local.series-file-shrink-ratio 0.3"
        "-storage.local.memory-chunks 2097152"
        "-storage.local.max-chunks-to-persist 1048576"
        "-storage.local.index-cache-size.fingerprint-to-metric 2097152"
        "-storage.local.index-cache-size.fingerprint-to-timerange 1048576"
        "-storage.local.index-cache-size.label-name-to-label-values 2097152"
        "-storage.local.index-cache-size.label-pair-to-fingerprints 41943040"
      ];
      alertmanagerURL = [ "http://localhost:9093" ];
      rules = [
        ''
          ALERT node_down
          IF up == 0
          FOR 5m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}}: Node is down.",
            description = "{{$labels.alias}} has been down for more than 5 minutes."
          }
          ALERT node_systemd_service_failed
          IF node_systemd_unit_state{state="failed"} == 1
          FOR 4m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}}: Service {{$labels.name}} failed to start.",
            description = "{{$labels.alias}} failed to (re)start service {{$labels.name}}."
          }
          ALERT node_filesystem_full_90percent
          IF sort(node_filesystem_free{device!="ramfs"} < node_filesystem_size{device!="ramfs"} * 0.1) / 1024^3
          FOR 5m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}}: Filesystem is running out of space soon.",
            description = "{{$labels.alias}} device {{$labels.device}} on {{$labels.mountpoint}} got less than 10% space left on its filesystem."
          }
          ALERT node_filesystem_full_in_4h
          IF predict_linear(node_filesystem_free{device!="ramfs"}[1h], 4*3600) <= 0
          FOR 5m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}}: Filesystem is running out of space in 4 hours.",
            description = "{{$labels.alias}} device {{$labels.device}} on {{$labels.mountpoint}} is running out of space of in approx. 4 hours"
          }
          ALERT node_filedescriptors_full_in_3h
          IF predict_linear(node_filefd_allocated[1h], 3*3600) >= node_filefd_maximum
          FOR 20m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}} is running out of available file descriptors in 3 hours.",
            description = "{{$labels.alias}} is running out of available file descriptors in approx. 3 hours"
          }
          ALERT node_load1_90percent
          IF node_load1 / on(alias) count(node_cpu{mode="system"}) by (alias) >= 0.9
          FOR 1h
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}}: Running on high load.",
            description = "{{$labels.alias}} is running with > 90% total load for at least 1h."
          }
          ALERT node_cpu_util_90percent
          IF 100 - (avg by (alias) (irate(node_cpu{mode="idle"}[5m])) * 100) >= 90
          FOR 1h
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}}: High CPU utilization.",
            description = "{{$labels.alias}} has total CPU utilization over 90% for at least 1h."
          }
          ALERT node_ram_using_90percent
          IF node_memory_MemFree + node_memory_Buffers + node_memory_Cached < node_memory_MemTotal * 0.1
          FOR 30m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary="{{$labels.alias}}: Using lots of RAM.",
            description="{{$labels.alias}} is using at least 90% of its RAM for at least 30 minutes now.",
          }
        ''
      ];
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
