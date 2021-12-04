# needs:
#   binary_sensor.lounge_ampel_status
#   light.lounge_ampel_licht_rot

let
  glados = import ../lib;
in
{
  services.home-assistant.config.automation =
  [
    {
      alias = "Ampel Rotes Licht";
      initial_state = true;
      trigger = {
        platform = "state";
        entity_id = "binary_sensor.lounge_ampel_status";
      };
      action = { service = "light.turn_on";
        data.entity_id = "light.lounge_ampel_licht_rot";
      };
    }
  ];
}
