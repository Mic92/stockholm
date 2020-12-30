# Needs:
#  sun.sunset
#  switch.lounge_diskoschalter_relay
let
  glados = import ../lib;
  disko_schalter = "switch.lounge_diskoschalter_relay";
  player = "media_player.lounge";
in
{
  services.home-assistant.config.automation =
  [
    { alias = "Party um 21 Uhr";
      trigger = {
        platform = "sun";
        event = "sunset";
      };
      action =
      ( glados.say.kiosk "Die Sonne geht unter. Und jetzt geht die Party im shack erst richtig los. Partybeleuchtung, aktiviert!" )
      ++
      [
        {
          service = "homeassistant.turn_on";
          entity_id = disko_schalter;
        }
        {
          service = "media_player.turn_on";
          data.entity_id = player;
        } # TODO: also start playlist if nothing is running?
      ];
    }
  ];
}
