{pkgs, config, ...}:
with import <stockholm/lib>;
{
  lass.telegraf = {
    enable = true;
    outputs = ''
      [outputs.influxdb]
        urls = ["http://prism:8086"]
        database = "all_data"
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
    ];
  };
  systemd.services.telegraf.path = with pkgs; [
    iputils
    lm_sensors
  ];
}
