# needs:
#   binary_sensor.portal_lock
#   sensor.keyholder
#   media_player.lounge

# additional state required on:
#  mpd.shack:
#     playlist "ansage"
#     playlist "lassulus"
#  lounge.kiosk.shack:
#     playlist "ansage"

let
  glados = import ../lib;
in
{
  services.home-assistant.config.automation =
  [
    {
      alias = "Bedanken bei Übernahme von Key";
      initial_state = true;
      trigger = {
        platform = "state";
        entity_id = "sensor.keyholder";
      };
      condition = {
        condition = "template";
        value_template = "{{ (trigger.from_state.state != 'No Keyholder') and (trigger.from_state.state != 'No Keyholder') }}";
      };
      action = glados.say.kiosk "Danke {{ trigger.to_state.state }} für das Übernehmen des Keys von {{ trigger.from_state.state }}";
    }
    {
      alias = "Keyholder Begrüßen wenn MPD hoch fährt";
      initial_state = true;
      trigger = {
        platform = "state";
        from = "unavailable";
        entity_id = "media_player.kiosk";
      };
      action = glados.say.kiosk (builtins.readFile ./announcement.j2);
    }
    {
      alias = "Start Music on portal lock on";
      trigger = {
        platform = "state";
        entity_id = "binary_sensor.portal_lock";
        to = "on";
        for.seconds = 30;
      };
      condition = {
        condition = "and";
        conditions =
        [
          { # only start if a keyholder opened the door and if the lounge mpd is currently not playing anything
            condition = "template";
            value_template = "{{ state('sensor.keyholder') != 'No Keyholder' }}";
          }
          {
            condition = "state";
            entity_id = "media_player.lounge";
            state = "idle";
          }
        ];
      };
      action =  [
          {
            service = "media_player.volume_set";
            data = {
              entity_id = "media_player.lounge";
              volume_level = 1.0;
            };
          }
          {
            service = "media_player.play_media";
            data = {
              entity_id = "media_player.lounge";
              media_content_type = "playlist";
              media_content_id = "ansage";
            };
          }
          { delay.seconds = 8.5; }
          {
            service = "media_player.volume_set";
            data = {
              entity_id = "media_player.lounge";
              volume_level = 0.6;
            };
          }
          {
            service = "media_player.play_media";
            data = {
              entity_id = "media_player.lounge";
              media_content_type =  "playlist";
              media_content_id = "lassulus";
            };
          }
        ];
    }
  ];
}
