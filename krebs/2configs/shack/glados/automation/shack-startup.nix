# needs:
#   binary_sensor.portal_lock
#   sensor.keyholder
#   media_player.lounge
let
  glados = import ../lib;
in
[
  {
    alias = "Greet new keyholder for key exchange";
    initial_state = true;
    trigger = {
      platform = "state";
      entity_id = "sensor.keyholder";
    };
    condition = {
      condition = "template";
      value_template = "{{ trigger.from_state.state != 'No Keyholder' }}";
    };
    #action = glados.say.lounge "Danke {{trigger.to_state.state}} für das Übernehmen des Keys von {{trigger.from_state.state}}";
    action = [];
  }

  {
    alias = "Start Music on portal lock on";
    # TODO: use "power" trigger
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
        { delay.seconds = 8; }
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
            media_content_id = "lassulus superradio";
          };
        }
      ];
  }
]
