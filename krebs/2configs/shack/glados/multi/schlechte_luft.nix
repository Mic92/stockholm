let
  glados = import ../lib;
  feinstaub_sensor = "sensor.fablab_particulate_matter_2_5um_concentration";
  ledring = "light.fablab_led_ring";
in
{
  services.home-assistant.config =
  {
    automation =
      [
      { alias = "Gute Luft Fablab";
        trigger = [
          {
            platform = "numeric_state";
            entity_id = feinstaub_sensor;
            below = 3;
          }
        ];
        action =
          [
            { service = "light.turn_on";
              data = {
                entity_id = ledring;
                effect = "Twinkle";
                color_name = "green";
              };
            }
          ];
      }
      { alias = "mäßige Luft Fablab";
        trigger = [
          {
            platform = "numeric_state";
            above = 3;
            below = 10;
            entity_id = feinstaub_sensor;
          }
        ];
        action =
          [
            { service = "light.turn_on";
              data = {
                entity_id = ledring;
                effect = "Twinkle";
                color_name = "yellow";
              };
            }
          ];
      }
      { alias = "schlechte Luft Fablab";
        trigger = [
          {
            platform = "numeric_state";
            above = 10;
            entity_id = feinstaub_sensor;
          }
        ];
        action =
          [
            { service = "light.turn_on";
              data = {
                entity_id = ledring;
                effect = "Fireworks";
                color_name = "red";
              };
            }
          ];
      }
      { alias = "Luft Sensor nicht verfügbar";
        trigger = [
          {
            platform = "state";
            to = "unavailable";
            entity_id = feinstaub_sensor;
          }
        ];
        action =
          [
            { service = "light.turn_on";
              data = {
                entity_id = ledring;
                effect = "Rainbow";
                color_name = "blue";
              };
            }
          ];
      }
      { alias = "Fablab Licht Reboot";
        trigger = [
          {
            platform = "state";
            from = "unavailable";
            entity_id = ledring;
          }
        ];
        action =
          [
            { service = "light.turn_on";
              data = {
                entity_id = ledring;
                effect = "Rainbow";
                color_name = "orange";
              };
            }
          ];
      }
    ];
  };
}
