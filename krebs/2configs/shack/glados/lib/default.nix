let
  prefix = "glados";
in
{

  say = let
    # returns a list of actions to be performed on an mpd to say something
    tts = { message, entity }:
    [
      {
        service = "media_player.turn_on";
        data.entity_id = "media_player.${entity}";
      }
      { service = "media_player.play_media";
        data = {
          entity_id = "media_player.${entity}";
          media_content_type = "playlist";
          media_content_id = "ansage";
        };
      }
      {
        service = "media_player.turn_on";
        data.entity_id = "media_player.${entity}";
      }
      { delay.seconds = 8; }
      { service = "tts.say";
        entity_id =  "media_player.${entity}";
        data_template = {
          inherit message;
          language = "de";
        };
      }
    ];
  in
  {
    lounge = message: tts {
      inherit message;
      entity = "lounge";
    };
    herrenklo = message: tts {
      inherit message;
      entity = "herrenklo";
    };
    kiosk = message: tts {
      inherit message;
      entity = "kiosk";
    };
  };
  tasmota =
  {
    plug = {host, name ? host, topic ? host}:
    {
      platform = "mqtt";
      inherit name;
      state_topic = "sonoff/stat/${topic}/POWER1";
      command_topic = "sonoff/cmnd/${topic}/POWER1";
      availability_topic = "sonoff/tele/${topic}/LWT";
      payload_on= "ON";
      payload_off= "OFF";
      payload_available= "Online";
      payload_not_available= "Offline";
      retain = false;
      qos = 1;
    };
  };
}
