# needs:
#  light.fablab_led
[
  { alias = "State on HA start-up";
  trigger = {
      platform = "homeassistant";
      event = "start";
    };
    action = [
      { service = "light.turn_on";
        data = {
          entity_id = "light.fablab_led";
          effect = "Rainbow";
          color_name = "yellow";
        };
      }
    ];
  }
]

