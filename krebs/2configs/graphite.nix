{ config, lib, pkgs, ... }:

# graphite-web on port 8080
# carbon cache on port 2003 (tcp/udp)

# TODO: krebs.graphite.minimal.enable
# TODO: configure firewall
with import <stockholm/lib>;
{
  imports = [ ];

  services.graphite = {
    api = {
      enable = true;
      listenAddress = "0.0.0.0";
    };
    carbon = {
      enableCache = true;
      # save disk usage by restricting to 1 bulk update per second
      config = ''
        [cache]
        MAX_CACHE_SIZE = inf
        MAX_UPDATES_PER_SECOND = 1
        MAX_CREATES_PER_MINUTE = 50
        MAX_UPDATES_PER_SECOND_ONSHUTDOWN = 9001

        LOG_CACHE_HITS = False
        LOG_CACHE_QUEUE_SORTS = False
        LOG_UPDATES = False
        LOG_LISTENER_CONNECTIONS = False
        LOG_CREATES = True
        '';
      storageAggregation = ''
      '';
      storageSchemas = ''
        [carbon]
        pattern = ^carbon\.
        retentions = 60:90d


        [radiation_sensor]
        pattern = ^sensors\.radiation\.
        retentions = 1m:30d,5m:180d,10m:3y

        [motion_sensors]
        pattern = ^sensors\.motion\.
        retentions = 1s:1h,60s:30d,300s:1y

        [motion_sensors]
        pattern = ^retiolum\.
        retentions = 10s:1h,30s:30d,300s:1y

        [homeassistant]
        pattern = ^homeassistant\.
        retentions = 10s:24h,30s:30d,300s:1y,3600s:5y

        [ara]
        pattern = ^ara\.
        retentions = 60s:30d,300s:1y

        [openweathermap]
        pattern = ^weather\.openweathermap
        retentions = 30m:30d,1h:5y

        [stadtklima]
        pattern = ^weather\.stadtklima-stuttgart
        retentions = 15m:30d,30m:5y

        [sensebox]
        pattern = ^weather\.sensebox
        retentions = 1m:90d,30m:5y

        [elchos]
        pattern = ^elchos\.
        retentions = 10s:14d,1m:90d,10m:5y

        [icinga_default]
        pattern = ^icinga
        retentions = 10s:14d,5m:90d,10m:5y

        [icinga_internals]
        pattern = ^icinga.*\.(max_check_attempts|reachable|current_attempt|execution_time|latency|state|state_type)
        retentions = 5m:7d

        [default]
        pattern = .*
        retentions = 60s:30d,300s:1y
        '';
    };
  };
  systemd.services.carbonCache.serviceConfig.Restart="always";
  systemd.services.graphiteApi.serviceConfig.Restart="always";
}
