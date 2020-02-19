let
  glados = import ../lib;
in
{
  # LED
  light = [
    (glados.esphome.led { name = "Fablab LED"; host = "fablab_led"; topic = "led_ring"; })

    (glados.esphome.led { name = "Fablab LED Part A"; host = "fablab_led"; topic = "A";})
    (glados.esphome.led { name = "Fablab LED Part B"; host = "fablab_led"; topic =  "B";})
    (glados.esphome.led { name = "Fablab LED Part C"; host = "fablab_led"; topic = "C";})
    (glados.esphome.led { name = "Fablab LED Part D"; host = "fablab_led"; topic = "D";})
  ];
  sensor = [
    (glados.esphome.ip { host = "fablab_feinstaub";})
    (glados.esphome.wifi { host = "fablab_feinstaub";})
    (glados.esphome.temp { host = "fablab_feinstaub";})
    (glados.esphome.dust_25m  { host = "fablab_feinstaub";})
    (glados.esphome.dust_100m { host = "fablab_feinstaub";})

    (glados.esphome.ip { host = "fablab_led";})
    (glados.esphome.wifi { host = "fablab_led";})

    (glados.esphome.ip { host = "rz_feinstaub";})
    (glados.esphome.wifi { host = "rz_feinstaub";})
    (glados.esphome.temp { host = "rz_feinstaub";})
    (glados.esphome.hum { host = "rz_feinstaub";})
    (glados.esphome.dust_25m  { host = "rz_feinstaub";})
    (glados.esphome.dust_100m { host = "rz_feinstaub";})
  ];
  automation =
    [
    { alias = "Gute Luft Fablab";
      trigger = [
        {
          platform = "numeric_state";
          below = 25;
          entity_id = "sensor.fablab_feinstaub_2_5um";
        }
      ];
      action =
        [
          { service = "light.turn_on";
            data = {
              entity_id = "light.fablab_led";
              effect = "Twinkle";
              color_name = "green";
            };
          }
        ];
    }
    { alias = "mäßige Luft Fablab";
      trigger = [
        #{
        #  platform = "numeric_state";
        #  above = 25;
        #  entity_id = "sensor.fablab_feinstaub_25m";
        #}
        {
          platform = "numeric_state";
          above = 25;
          below = 50;
          entity_id = "sensor.fablab_feinstaub_2_5um";
        }
      ];
      action =
        [
          { service = "light.turn_on";
            data = {
              entity_id = "light.fablab_led";
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
          above = 50;
          entity_id = "sensor.fablab_feinstaub_2_5um";
        }
      ];
      action =
        [
          { service = "light.turn_on";
            data = {
              entity_id = "light.fablab_led";
              effect = "Twinkle";
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
          entity_id = "sensor.fablab_feinstaub_2_5um";
        }
      ];
      action =
        [
          { service = "light.turn_on";
            data = {
              entity_id = "light.fablab_led";
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
          entity_id = "light.fablab_led";
        }
      ];
      action =
        [
          { service = "light.turn_on";
            data = {
              entity_id = "light.fablab_led";
              effect = "Rainbow";
              color_name = "orange";
            };
          }
        ];
    }
  ];
}
