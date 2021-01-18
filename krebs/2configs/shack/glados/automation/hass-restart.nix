# needs:
#  light.fablab_led
{
  services.home-assistant.config.automation =
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
  ];
}

