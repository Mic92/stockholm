let
  lib =  import <nixpkgs/lib>;
  prefix = "glados";
in
{
  esphome =
  {
    temp = name:
    {
      platform = "mqtt";
      name = "${name} Temperature";
      device_class = "temperature";
      state_topic = "${prefix}/${name}/sensor/temperature/state";
      availability_topic = "${prefix}/${name}/status";
      payload_available = "online";
      payload_not_available = "offline";
    };
    hum = name:
    {
      platform = "mqtt";
      device_class = "humidity";
      name = "${name} Humidity";
      state_topic = "${prefix}/${name}/sensor/humidity/state";
      availability_topic = "${prefix}/${name}/status";
      payload_available = "online";
      payload_not_available = "offline";
    };
  };
  tasmota =
  {
    plug = name: topic:
    {
      platform = "mqtt";
      inherit name;
      state_topic = "sonoff/stat/${topic}/POWER1";
      command_topic = "sonoff/cmnd/${topic}/POWER1";
      availability_topic = "sonoff/tele/${topic}/LWT";
      payload_on= "ON";
      payload_off= "OFF";
      payload_available= "Online";
      payload_not_available= "Offline";
      retain = false;
      qos = 1;
    };
  };
}
