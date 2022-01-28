{config, pkgs, lib, ...}: let

  unstable-pkgs = import <nixpkgs-unstable> {};

in {
  # symlink the zigbee controller
  services.udev.extraRules = ''
    SUBSYSTEM=="tty", ATTRS{idVendor}=="0451", ATTRS{idProduct}=="16a8", SYMLINK+="cc2531", MODE="0660", GROUP="dialout"
    SUBSYSTEM=="tty", ATTRS{idVendor}=="10c4", ATTRS{idProduct}=="ea60", SYMLINK+="cc2652", MODE="0660", GROUP="dialout"
  '';

  # needed to use unstable package
  systemd.services.zigbee2mqtt.environment.ZIGBEE2MQTT_DATA = "/var/lib/zigbee2mqtt";

  services.zigbee2mqtt = {
    enable = true;
    package = unstable-pkgs.zigbee2mqtt;
    settings = {
      homeassistant = true;
      frontend.port = 1337;
      experimental.new_api = true;
      permit_join = false;
      mqtt = {
        discovery = true;
        base_topic = "zigbee";
        server = "mqtt://10.42.0.1";
        user = "gg23";
        password = "gg23-mqtt";
      };
      serial = {
        port = "/dev/cc2652";
        # disable_led = true;
      };
      advanced = {
        pan_id = 4222;
      };
    };
  };

  services.home-assistant.config = {
    sensor = [
      # Sensor for monitoring the bridge state
      {
        platform = "mqtt";
        name = "Zigbee2mqtt Bridge state";
        state_topic = "/zigbee2mqtt/bridge/state";
        icon = "mdi:router-wireless";
      }
      # Sensor for Showing the Zigbee2mqtt Version
      {
        platform = "mqtt";
        name = "Zigbee2mqtt Version";
        state_topic = "/zigbee2mqtt/bridge/config";
        value_template = "{{ value_json.version }}";
        icon = "mdi:zigbee";
      }
      # Sensor for Showing the Coordinator Version
      {
        platform = "mqtt";
        name = "Coordinator Version";
        state_topic = "/zigbee2mqtt/bridge/config";
        value_template = "{{ value_json.coordinator }}";
        icon = "mdi:chip";
      }
    ];
    switch = [
      {
        platform = "mqtt";
        name = "zigbee2mqtt_join";
        state_topic = "/zigbee2mqtt/bridge/config/permit_join";
        command_topic = "/zigbee2mqtt/bridge/config/permit_join";
        payload_on = "true";
        payload_off = "false";
      }
    ];
    automation = [
      #{
      #  alias = "Zigbee2mqtt Log Level";
      #  initial_state = "on";
      #  trigger = {
      #    platform = "state";
      #    entity_id = "input_select.zigbee2mqtt_log_level";
      #  };
      #  action = [
      #    {
      #      service =  "mqtt.publish";
      #      data = {
      #        payload_template = "{{ states('input_select.zigbee2mqtt_log_level') }}";
      #        topic =  "/zigbee2mqtt/bridge/config/log_level";
      #      };
      #    }
      #  ];
      #}
      # Automation to start timer when enable join is turned on
      {
        id = "zigbee_join_enabled";
        alias = "";
        trigger = {
          platform = "state";
          entity_id = "switch.zigbee2mqtt_join";
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
        trigger = [
          {
            platform = "event";
            event_type = "timer.finished";
            event_data.entity_id = "timer.zigbee_permit_join";
          }
          {
            platform = "state";
            entity_id = "switch.zigbee2mqtt_join";
            to = "off";
          }
        ];
        action = [
          { service = "timer.cancel";
            data.entity_id = "timer.zigbee_permit_join";
          }
          { service = "switch.turn_off";
            entity_id = "switch.zigbee2mqtt_join";
          }
        ];
      }
    ];
    #input_select.zigbee2mqtt_log_level = {
    #  name = "Zigbee2mqtt Log Level";
    #  options = [
    #    "debug"
    #    "info"
    #    "warn"
    #    "error"
    #  ];
    #  initial = "info";
    #  icon = "mdi:format-list-bulleted";
    #};

    timer.zigbee_permit_join = {
      name = "Zigbee Time remaining";
      duration = 120;
    };
  };
}

