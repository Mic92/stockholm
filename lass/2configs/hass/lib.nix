{
  lights = {
    bett = "light.0x0017880106ed3bd8_light";
    essen = "light.0x0017880108327622_light";
    arbeit = "light.0x0017880106ee2865_light";
    nass = "light.0x00178801082e9f2f_light";
  };

  sensors = {
    bett = "0x00178801086ac38c";
  };

  lightswitch = switch: light: {
    automation = [
      {
        trigger = {
          platform = "mqtt";
          topic = "zigbee/${switch}";
        };
        condition = {
          condition = "or";
          conditions = [
            {
              condition = "template";
              value_template = "{{ trigger.payload_json.action == 'on-press' }}";
            }
            {
              condition = "template";
              value_template = "{{ trigger.payload_json.action == 'up-press' }}";
            }
            {
              condition = "and";
              conditions = [
                {
                  condition = "template";
                  value_template = "{{ trigger.payload_json.action == 'down-press' }}";
                }
                {
                  condition = "template";
                  value_template = "{{ trigger.payload_json.brightness > 30 }}";
                }
              ];
            }
          ];
        };
        action = [
          {
            service = "light.turn_on";
            data_template = {
              entity_id = light;
              brightness = "{{ trigger.payload_json.brightness }}";
            };
          }
        ];
      }
      {
        trigger = {
          platform = "mqtt";
          topic = "zigbee/${switch}";
        };
        condition = {
          condition = "or";
          conditions = [
            {
              condition = "template";
              value_template = "{{ trigger.payload_json.action == 'off-press' }}";
            }
            {
              condition = "template";
              value_template = "{{ trigger.payload_json.brightness < 30 }}";
            }
          ];
        };
        action = {
          service = "light.turn_off";
          data_template = {
            entity_id = light;
          };
        };
      }
    ];
  };
}
