{ pkgs, lib, config, ... }:
# from https://gist.github.com/globin/02496fd10a96a36f092a8e7ea0e6c7dd
{
  imports = [
    ./alert-rules.nix
    ./irc-hooks.nix
  ];
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
      scrapeConfigs = [
        {
          job_name = "node";
          scrape_interval = "10s";
          static_configs = [
            {
              targets = [
                "wolf.shack:9100"
              ];
              labels = {
                alias = "wolf.shack";
              };
            }
            {
              targets = [
                "infra01.shack:9100"
              ];
              labels = {
                alias = "infra01.shack";
              };
            }
            {
              targets = [
                "unifi.shack:9130"
              ];
              labels = {
                alias = "unifi.shack";
              };
            }
            {
              targets = [
                "puyak.shack:9100" # puyak.shack
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
        {
          job_name = "blackbox";
          metrics_path = "/probe";
          params.module = [ "icmp" ];
          static_configs = [
            {
              targets = [
                "google.com"
                "wolf.shack"
                "web.de"
                "10.0.0.1"
                "licht.shack"
              ];
            }
          ];
          relabel_configs = [
            {
              source_labels = ["__address__"];
              target_label = "__param_target";
            }
            {
              source_labels = ["__param_target"];
              target_label = "instance";
            }
            {
              target_label = "__address__";
              replacement = "127.0.0.1:9115";
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
        listenAddress = "127.0.0.1";
        webExternalUrl = "http://alert.prometheus.shack";
        logLevel = "debug";

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
            "receiver" = "shack-admins";
          };
          "receivers" = [
            {
              "name" = "shack-admins";
              "email_configs" = [ ];
              "webhook_configs" = [
                {
                  "url" = "http://localhost:16320";
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
