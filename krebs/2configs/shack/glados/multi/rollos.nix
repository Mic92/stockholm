# 

let
  glados = import ../lib;
  tempsensor = "sensor.dark_sky_temperature";
  all_covers = [
    "cover.crafting_rollo"
    "cover.elab_rollo"
    "cover.or2_rollo"
    "cover.retroraum_rollo"
  ];
in
{
  services.home-assistant.config =
  {
    automation =
    [
      { alias = "Rollos fahren Runter";
        trigger = [
          {
            platform = "numeric_state";
            entity_id = tempsensor;
            above = 25;
            for = "00:30:00";
          }
        ];
        condition =
          [
            {
              condition = "state";
              entity_id = "sun.sun";
              state = "above_horizon";
            }
        ];
        action =
          [
            { service = "cover.close_cover";
              entity_id = all_covers;
            }
          ];
      }
      { alias = "Rollos fahren Hoch";
        trigger = [
          {
            platform = "sun";
            event = "sunset";
          }
        ];
        condition = [ ];
        action =
          [
            { service = "cover.open_cover";
              entity_id = all_covers;
            }
          ];
      }
    ];
  };
}
