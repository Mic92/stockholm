# uses:
#  switch.crafting_giesskanne_relay
let
  glados = import ../lib;
  seconds = 20;
  wasser = "switch.crafting_giesskanne_relay";
  brotbox = {
    minutes = 10;
    pump = "switch.crafting_brotbox_pumpe";
    sensor = "sensor.statistics_for_sensor_crafting_brotbox_soil_moisture";
  };
in
{
  sensor = map ( entity_id: {
      platform = "statistics";
      name = "Statistics for ${entity_id}";
      inherit entity_id;
      max_age.minutes = "60";
      sampling_size = 1000;
    }) ["sensor.crafting_brotbox_soil_moisture"];


  automation =
  [
    ### Brotbox #####
    #{ alias = "Brotbox: water for ${toString brotbox.minutes} minutes every hour";
    #  trigger =
    #  { # Trigger once every hour at :42
    #    platform = "time_pattern";
    #    minutes = 42;
    #  };
    #  condition = {
    #    condition = "numeric_state";
    #    entity_id = brotbox.sensor;
    #    value_template = "{{ state_attr('${brotbox.sensor}', 'median') }}";
    #    below = 75;
    #  };
    #  action =
    #  [
    #    {
    #      service = "homeassistant.turn_on";
    #      entity_id = brotbox.pump;
    #    }
    #    { delay.minutes = brotbox.minutes; }
    #    {
    #      service = "homeassistant.turn_off";
    #      entity_id =  brotbox.pump ;
    #    }
    #  ];
    #}
    { alias = "Brotbox: Always turn off water after ${toString (brotbox.minutes * 2)} minutes";
      trigger =
      {
        platform = "state";
        entity_id = brotbox.pump;
        to = "on";
        for.minutes = brotbox.minutes*2;
      };
      action =
      {
        service = "homeassistant.turn_off";
        entity_id =  brotbox.pump;
      };
    }

    ##### Kaffeemaschine
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
            wasser
          ];
        }
        { delay.seconds = seconds; }
        {
          service = "homeassistant.turn_off";
          entity_id =  [
            wasser
          ];
        }
      ];
    }
    { alias = "Always turn off water after ${toString (seconds * 2)}seconds";
      trigger = [
        {
          platform = "state";
          entity_id = wasser;
          to = "on";
          for.seconds = seconds*2;
        }
      ];
      action =
      [
        {
          service = "homeassistant.turn_off";
          entity_id =  [ wasser ];
        }
      ];
    }
  ];
}
