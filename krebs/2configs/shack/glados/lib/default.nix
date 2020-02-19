let
  prefix = "glados";
in
{

  say = let
    # returns a list of actions to be performed on an mpd to say something
    tts = { message, entity }:
    [
      {
        service = "media_player.turn_on";
        data.entity_id = "media_player.${entity}";
      }
      { service = "media_player.play_media";
        data = {
          entity_id = "media_player.${entity}";
          media_content_type = "playlist";
          media_content_id = "ansage";
        };
      }
      {
        service = "media_player.turn_on";
        data.entity_id = "media_player.${entity}";
      }
      { delay.seconds = 8; }
      { service = "tts.say";
        entity_id =  "media_player.${entity}";
        data_template = {
          inherit message;
          language = "de";
        };
      }
    ];
  in
  {
    lounge = message: tts {
      inherit message;
      entity = "lounge";
    };
    herrenklo = message: tts {
      inherit message;
      entity = "herrenklo";
    };
    kiosk = message: tts {
      inherit message;
      entity = "kiosk";
    };
  };
  esphome =
  {
    temp = {host, topic ? "temperature" }:
    {
      platform = "mqtt";
      name = "${host} Temperature";
      device_class = "temperature";
      unit_of_measurement =  "°C";
      icon = "mdi:thermometer";
      state_topic = "${prefix}/${host}/sensor/${topic}/state";
      availability_topic = "${prefix}/${host}/status";
      payload_available = "online";
      payload_not_available = "offline";
    };
    hum = {host, topic ? "humidity" }:
    {
      platform = "mqtt";
      unit_of_measurement = "%";
      icon = "mdi:water-percent";
      device_class = "humidity";
      name = "${host} Humidity";
      state_topic = "${prefix}/${host}/sensor/${topic}/state";
      availability_topic = "${prefix}/${host}/status";
      payload_available = "online";
      payload_not_available = "offline";
    };
    rollo = {host, topic ? "rollo" }:
    {
      optimistic = true;
      platform = "mqtt";
      name = "${host} Rollo";
      state_topic = "${prefix}/${host}/sensor/${topic}/state";
      command_topic = "${prefix}/${host}/sensor/${topic}/command";
      availability_topic = "${prefix}/${host}/status";
      position_topic =  "${prefix}/${host}/cover/${topic}/position/state";
      set_position_topic = "${prefix}/${host}/cover/${topic}/position/command";
    };
    # copied from "homeassistant/light/fablab_led/led_ring/config"
    led = {host,  topic ? "led", name ? host}:
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
    monoled = {host,  topic ? "blue_led", name ? host "MonoLED ${host}"}:
    {
      platform = "mqtt";
      inherit name;
      schema = "json";
      brightness =  true;
      effect =  true;
      effect_list = [ # TODO: may be different
        "Strobe"
        "Twinkle"
        "None"
      ];
      state_topic = "${prefix}/${host}/light/${topic}/state";
      command_topic = "${prefix}/${host}/light/${topic}/command";
      availability_topic = "${prefix}/${host}/status";
    };
    btn = {host, topic ? "button", name ? "${host} ${topic}"}: #binary_sensor
    {
      platform = "mqtt";
      name = "${host} Button";
      state_topic = "${prefix}/${host}/binary_sensor/${topic}/state";
      availability_topic = "${prefix}/${host}/status";
    };
    relay = {host, name ? "${host} ${topic}", topic ? "relay" }: #switch
    {
      inherit name;
      platform = "mqtt";
      state_topic = "${prefix}/${host}/switch/${topic}/state";
      command_topic = "${prefix}/${host}/switch/${topic}/command";
      availability_topic = "${prefix}/${host}/status";
    };
    # Feinstaub
    dust_25m = { host, name ? "${host} < 2.5µm", topic ? "particulate_matter_25m_concentration" }:
    {
      platform = "mqtt";
      unit_of_measurement = "µg/m³";
      icon = "mdi:chemical-weapon";
      inherit name;
      state_topic = "${prefix}/${host}/sensor/${topic}/state";
      availability_topic = "${prefix}/${host}/status";
    };
    dust_100m = {host, name ? "${host} < 10µm", topic ? "particulate_matter_100m_concentration" }:
    {
      platform = "mqtt";
      unit_of_measurement = "µg/m³";
      icon = "mdi:chemical-weapon";
      inherit name;
      state_topic = "${prefix}/${host}/sensor/${topic}/state";
      availability_topic = "${prefix}/${host}/status";
    };
    ip = {host, name ? "${host} IP", topic ? "ip_address" }:
    {
      platform = "mqtt";
      inherit name;
      state_topic = "${prefix}/${host}/sensor/${topic}/state";
      availability_topic = "${prefix}/${host}/status";
    };
    wifi = {host, name ? "${host} Wifi Signal", topic ? "wifi_signal" }:
    {
      platform = "mqtt";
      unit_of_measurement = "dB";
      icon = "mdi:wifi";
      inherit name;
      state_topic = "${prefix}/${host}/sensor/${topic}/state";
      availability_topic = "${prefix}/${host}/status";
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
    plug = {host, name ? host, topic ? host}:
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
