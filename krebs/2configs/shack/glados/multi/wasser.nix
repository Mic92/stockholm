let
  tasmota_plug = name: topic:
  { platform = "mqtt";
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
  seconds = 10;
in
{
  switch = [
    (tasmota_plug "Wasser" "plug")
  ];
  automation =
  [
    { alias = "Water the plant for ${toString seconds} seconds";
      trigger = [
        { # trigger at 20:00 no matter what
          # TODO: retry or run only if switch.wasser is available
          platform = "time";
          at = "20:00:00";
        }
      ];
      action =
      [
        {
          service = "homeassistant.turn_on";
          entity_id =  [
            "switch.wasser"
          ];
        }
        { delay.seconds = seconds; }
        {
          service = "homeassistant.turn_off";
          entity_id =  [
            "switch.wasser"
          ];
        }
      ];
    }
    { alias = "Always turn off water after ${toString (seconds * 2)}seconds";
      trigger = [
        {
          platform = "state";
          entity_id = "switch.wasser";
          to = "on";
          for.seconds = seconds*2;
        }
      ];
      action =
      [
        {
          service = "homeassistant.turn_off";
          entity_id =  [ "switch.wasser" ];
        }
      ];
    }
  ];
}
