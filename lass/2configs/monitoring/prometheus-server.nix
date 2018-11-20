{ pkgs, lib, config, ... }:
{
  #networking = {
  #  firewall.allowedTCPPorts = [
  #    3000  # grafana
  #    9090  # prometheus
  #    9093  # alertmanager
  #  ];
  #  useDHCP = true;
  #};

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i retiolum -p tcp --dport 3000"; target = "ACCEPT"; }
    { predicate = "-i retiolum -p tcp --dport 9090"; target = "ACCEPT"; }
    { predicate = "-i retiolum -p tcp --dport 9093"; target = "ACCEPT"; }
  ];

  services = {
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
          ALERT node_swap_using_80percent
          IF node_memory_SwapTotal - (node_memory_SwapFree + node_memory_SwapCached) > node_memory_SwapTotal * 0.8
          FOR 10m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary="{{$labels.alias}}: Running out of swap soon.",
            description="{{$labels.alias}} is using 80% of its swap space for at least 10 minutes now."
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
              ] ++ map (host: "${host}:9100") (lib.attrNames (lib.filterAttrs (_: host: host.owner.name == "lass" && host.monitoring) config.krebs.hosts));
              #labels = {
              #  alias = "prometheus.example.com";
              #};
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
                }
              ];
              "webhook_configs" = [
                {
                  "url" = "http://127.0.0.1:14813/prometheus-alerts";
                  "send_resolved" = true;
                }
              ];
            }
          ];
        };
      };
    };
    grafana = {
      enable = true;
      addr = "0.0.0.0";
      domain = "grafana.example.com";
      rootUrl = "https://grafana.example.com/";
      auth.anonymous.enable = true;
      auth.anonymous.org_role = "Admin";
    };
  };
  services.logstash = {
    enable = true;
    inputConfig = ''
      http {
        port => 14813
        host => "127.0.0.1"
      }
    '';
    filterConfig = ''
      if ([alerts]) {
        ruby {
          code => '
            lines = []
            event["alerts"].each {|p|
              lines << "#{p["labels"]["instance"]}#{p["annotations"]["summary"]} #{p["status"]}"
            }
            event["output"] = lines.join("\n")
          '
        }
      }
    '';
    outputConfig = ''
      file { path => "/tmp/logs.json" codec => "json_lines" }
      irc {
        channels => [ "#noise" ]
        host => "irc.r"
        nick => "alarm"
        codec => "json_lines"
        format => "%{output}"
      }
    '';
    #plugins = [ ];
  };
}
