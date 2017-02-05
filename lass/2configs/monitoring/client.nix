{pkgs, config, ...}:
with import <stockholm/lib>;
{
  services.telegraf = {
    enable = true;

    extraConfig = {
      interval = "1s";
      outputs = {
        influxdb = {
          urls = ["http://prism:8086"];
          database = "telegraf_db";
          user_agent = "telegraf";
        };
      };
      inputs = {
        cpu = {
          percpu = false;
          totalcpu = true;
        };
        mem = {};
        net = {};
      };
    };
  };
}
