let
  prefix = "glados";
in
{
  esphome =
  {
    temp = {name, topic ? "temperature" }:
    {
      platform = "mqtt";
      name = "${name} Temperature";
      device_class = "temperature";
      unit_of_measurement =  "°C";
      icon = "mdi:thermometer";
      state_topic = "${prefix}/${name}/sensor/${topic}/state";
      availability_topic = "${prefix}/${name}/status";
      payload_available = "online";
      payload_not_available = "offline";
    };
    hum = {name, topic ? "humidity" }:
    {
      platform = "mqtt";
      unit_of_measurement = "%";
      icon = "mdi:water-percent";
      device_class = "humidity";
      name = "${name} Humidity";
      state_topic = "${prefix}/${name}/sensor/${topic}/state";
      availability_topic = "${prefix}/${name}/status";
      payload_available = "online";
      payload_not_available = "offline";
    };
    # copied from "homeassistant/light/fablab_led/led_ring/config"
    led = {name,  topic ? "led", host ? name }:
    { # name: fablab_led
      # topic: led_ring
      platform = "mqtt";
      inherit name;
      schema = "json";
      brightness =  true;
      rgb =  true;
      effect =  true;
      effect_list = [ # TODO: may be different
        "Random"
        "Strobe"
        "Rainbow"
        "Color Wipe"
        "Scan"
        "Twinkle"
        "Fireworks"
        "Addressable Flicker"
        "None"
      ];
      state_topic = "${prefix}/${host}/light/${topic}/state";
      command_topic = "${prefix}/${host}/light/${topic}/command";
      availability_topic = "${prefix}/${host}/status";
      payload_available = "online";
      payload_not_available = "offline";
      qos = 1;
    };
    # Feinstaub
    dust_25m = { host, name ? "${host} < 2.5µm", topic ? "particulate_matter_25m_concentration" }:
    {
      platform = "mqtt";
      unit_of_measurement = "µg/m³";
      icon = "mdi:chemical-weapon";
      inherit name;
      state_topic = "${prefix}/${host}/sensor/${topic}/state";
      availability_topic = "${prefix}/${name}/status";
    };
    dust_100m = {host, name ? "${host} < 10µm", topic ? "particulate_matter_100m_concentration" }:
    {
      platform = "mqtt";
      unit_of_measurement = "µg/m³";
      icon = "mdi:chemical-weapon";
      inherit name;
      state_topic = "${prefix}/${name}/sensor/${topic}/state";
      availability_topic = "${prefix}/${name}/status";
    };
    switch = {host, name ? "${host} Button", topic ? "btn" }:
    # host: ampel
    # name: Button 1
    # topic: btn1
    {
      inherit name;
      platform = "mqtt";
      state_topic = "${prefix}/${host}/sensor/${topic}/state";
      command_topic = "${prefix}/${host}/switch/${topic}/state";
      availability_topic = "${prefix}/${host}/status";
    };
  };
  tasmota =
  {
    plug = {name, topic ? name }:
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
