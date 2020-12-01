let
  power_x = name: phase:
  { platform = "mqtt";
    name = "${phase} ${name}";
    state_topic = "/power/total/${phase}/${name}";
    availability_topic = "/power/lwt";
    payload_available = "Online";
    payload_not_available = "Offline";
  };
  power_consumed =
  { platform = "mqtt";
    name = "Power Consumed";
    device_class = "power";
    state_topic = "/power/total/consumed";
    availability_topic = "/power/lwt";
    payload_available = "Online";
    payload_not_available = "Offline";
  };
  power_volt = power_x "Voltage";
  power_watt = (power_x "Power") ;
  power_curr = power_x "Current";
in
{
  services.home-assistant.config.sensor =
   (map power_volt [ "L1" "L2" "L3" ])
++ (map (x: ((power_watt x) // { device_class = "power"; })) [ "L1" "L2" "L3" ])
++ (map power_curr [ "L1" "L2" "L3" ])
++ [ power_consumed ];
}
