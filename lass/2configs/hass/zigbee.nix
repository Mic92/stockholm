{config, pkgs, lib, ...}: let

  zigbee2mqtt_cfg = pkgs.writeText "zigbee2mqtt.json" (builtins.toJSON {
    homeassistant = true;
    permit_join = true;
    mqtt = {
      discovery = true;
      base_topic = "zigbee";
      server = "mqtt://10.42.0.1";
      user = "gg23";
      password = "gg23-mqtt";
    };
    serial.port = "/dev/cc2531";
  });

in {
  # symlink the zigbee controller
  services.udev.extraRules = ''
    SUBSYSTEM=="tty", ATTRS{idVendor}=="0451", ATTRS{idProduct}=="16a8", SYMLINK+="cc2531", MODE="0660", GROUP="dailout"
  '';

  system.activationScripts.installZigbee = ''
    install -d /var/lib/zigbee2mqtt
    install ${zigbee2mqtt_cfg} /var/lib/zigbee2mqtt/configuration.yaml
  '';

  # hack to restart docker container on config change
  systemd.services.docker-zigbee2mqtt.environment.cfg = zigbee2mqtt_cfg;

  docker-containers.zigbee2mqtt = {
    image = "koenkk/zigbee2mqtt";
    extraDockerOptions = [
      "--device=/dev/cc2531:/dev/cc2531"
    ];
    volumes = ["/var/lib/zigbee2mqtt:/app/data"];
  };

  lass.hass.config = {
    sensor = [
      # Sensor for monitoring the bridge state
      {
        platform = "mqtt";
        name = "Zigbee2mqtt Bridge state";
        state_topic = "/zigbee/bridge/state";
        icon = "mdi:router-wireless";
      }
      # Sensor for Showing the Zigbee2mqtt Version
      {
        platform = "mqtt";
        name = "Zigbee2mqtt Version";
        state_topic = "/zigbee/bridge/config";
        value_template = "{{ value_json.version }}";
        icon = "mdi:zigbee";
      }
      # Sensor for Showing the Coordinator Version
      {
        platform = "mqtt";
        name = "Coordinator Version";
        state_topic = "/zigbee/bridge/config";
        value_template = "{{ value_json.coordinator }}";
        icon = "mdi:chip";
      }
    ];
    switch = [
      {
        platform = "mqtt";
        name = "Zigbee2mqtt Main join";
        state_topic = "/zigbee/bridge/config/permit_join";
        command_topic = "/zigbee/bridge/config/permit_join";
        payload_on = "true";
        payload_off = "false";
      }
    ];
    automation = [
      {
        alias = "Zigbee2mqtt Log Level";
        initial_state = "on";
        trigger = {
          platform = "state";
          entity_id = "input_select.zigbee2mqtt_log_level";
        };
        action = [
          {
            service =  "mqtt.publish";
            data = {
              payload_template = "{{ states('input_select.zigbee2mqtt_log_level') }}";
              topic =  "/zigbee/bridge/config/log_level";
            };
          }
        ];
      }
      # Automation to start timer when enable join is turned on
      {
        id = "zigbee_join_enabled";
        alias = "Zigbee Join Enabled";
        hide_entity = "true";
        trigger = {
          platform = "state";
          entity_id = "switch.zigbee2mqtt_main_join";
          to = "on";
        };
        action = {
          service = "timer.start";
          entity_id = "timer.zigbee_permit_join";
        };
      }
      # Automation to stop timer when switch turned off and turn off switch when timer finished
      {
        id = "zigbee_join_disabled";
        alias = "Zigbee Join Disabled";
        hide_entity = "true";
        trigger = [
          {
            platform = "event";
            event_type = "timer.finished";
            event_data.entity_id = "timer.zigbee_permit_join";
          }
          {
            platform = "state";
            entity_id = "switch.zigbee2mqtt_main_join";
            to = "off";
          }
        ];
        action = [
          { service = "timer.cancel";
            data.entity_id = "timer.zigbee_permit_join";
          }
          { service = "switch.turn_off";
            entity_id = "switch.zigbee2mqtt_main_join";
          }
        ];
      }
    ];
    input_select.zigbee2mqtt_log_level = {
      name = "Zigbee2mqtt Log Level";
      options = [
        "debug"
        "info"
        "warn"
        "error"
      ];
      initial = "info";
      icon = "mdi:format-list-bulleted";
    };

    timer.zigbee_permit_join = {
      name = "Zigbee Time remaining";
      duration = 120;
    };
  };
}

