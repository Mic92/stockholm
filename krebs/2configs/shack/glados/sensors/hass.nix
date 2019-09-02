let
  esphome_temp = name: 
  { platform = "mqtt";
    name = "${name} Temperature";
    device_class = "temperature";
    state_topic = "glados/${name}/sensor/temperature/state";
    availability_topic = "glados/${name}/status";
    payload_available = "online";
    payload_not_available = "offline";
  };
  esphome_hum = name:
  { platform = "mqtt";
    device_class = "humidity";
    name = "${name} Humidity";
    state_topic = "glados/${name}/sensor/humidity/state";
    availability_topic = "glados/${name}/status";
    payload_available = "online";
    payload_not_available = "offline";
  };
in
     (map esphome_temp [ "lounge" "werkstatt" "herrenklo" "dusche" "fablab" "whc" ])
  ++ (map esphome_hum  [ "lounge" "werkstatt" "herrenklo" "dusche" "fablab" "whc" ])
