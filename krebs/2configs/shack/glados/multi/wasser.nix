# uses:
#  switch.crafting_giesskanne_relay
let
  glados = import ../lib;
  seconds = 20;
  wasser = "switch.crafting_giesskanne_relay";
in
{
  switch = [
    (glados.tasmota.plug { host = "Wasser"; topic = "plug";} )
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
