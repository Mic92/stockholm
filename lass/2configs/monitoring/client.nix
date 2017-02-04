{pkgs, config, ...}:
with import <stockholm/lib>;
{
  lass.telegraf = {
    enable = true;
    interval = "1s";


    outputs = ''
      [outputs.influxdb]
        urls = ["http://prism:8086"]
        database = "telegraf_db"
        user_agent = "telegraf"
    '';
    inputs = [
      ''
        [cpu]
          percpu = false
          totalcpu = true
          drop = ["cpu_time"]
      ''
      ''
        [[inputs.mem]]
      ''
      ''
        [[inputs.ping]]
        urls = ["8.8.8.8"]
      ''
      ''
        [[inputs.net]]
      ''
    ];
  };
  systemd.services.telegraf.path = with pkgs; [
    iputils
    lm_sensors
  ];

  services.collectd = {
    enable = true;
    autoLoadPlugin = true;
    extraConfig = ''
      Hostname ${config.krebs.build.host.name}
      LoadPlugin load
      LoadPlugin disk
      LoadPlugin memory
      Interval 30.0

      LoadPlugin interface
      <Plugin "interface">
        Interface "*Link"
        Interface "lo"
        Interface "vboxnet*"
        Interface "virbr*"
        IgnoreSelected true
      </Plugin>

      LoadPlugin df
      <Plugin "df">
        MountPoint "/nix/store"
        FSType "tmpfs"
        FSType "binfmt_misc"
        FSType "debugfs"
        FSType "mqueue"
        FSType "hugetlbfs"
        FSType "systemd-1"
        FSType "cgroup"
        FSType "securityfs"
        FSType "ramfs"
        FSType "proc"
        FSType "devpts"
        FSType "devtmpfs"
        MountPoint "/var/lib/docker/devicemapper"
        IgnoreSelected true
      </Plugin>

      LoadPlugin cpu
      <Plugin cpu>
        ReportByCpu true
        ReportByState true
        ValuesPercentage true
      </Plugin>

      LoadPlugin network
      <Plugin "network">
          Server "prism" "25826"
      </Plugin>
    '';
  };
}
