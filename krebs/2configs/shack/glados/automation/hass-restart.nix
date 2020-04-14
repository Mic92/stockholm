# needs:
#  light.fablab_led
[
  { alias = "State on HA start-up";
  trigger = {
      platform = "homeassistant";
      event = "start";
    };
    # trigger good/bad air
    action = [
      { service = "light.turn_on";
        data = {
          entity_id = "light.fablab_led";
          effect = "Rainbow";
          color_name = "purple";
        };
      }
    ];
  }
]

