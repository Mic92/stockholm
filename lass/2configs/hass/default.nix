{ config, lib, pkgs, ... }:
with import ./lib.nix { inherit lib; };

{
  imports = [
    ./zigbee.nix
    ./rooms/bett.nix
    ./rooms/essen.nix
    ./rooms/nass.nix
  ];

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i int0 -p tcp --dport 1883"; target = "ACCEPT"; } # mosquitto
    { predicate = "-i docker0 -p tcp --dport 1883"; target = "ACCEPT"; } # mosquitto
    { predicate = "-i int0 -p tcp --dport 8123"; target = "ACCEPT"; } # hass
    { predicate = "-i int0 -p tcp --dport 1337"; target = "ACCEPT"; } # hass
    { predicate = "-i retiolum -p tcp --dport 8123"; target = "ACCEPT"; } # hass
    { predicate = "-i retiolum -p tcp --dport 1337"; target = "ACCEPT"; } # hass frontend
    { predicate = "-i wiregrill -p tcp --dport 8123"; target = "ACCEPT"; } # hass
  ];

  services.home-assistant = {
    enable = true;
    package = pkgs.home-assistant.override {
      # extraComponents = [ "hue" ];
    };
    configWritable = true;
    lovelaceConfigWritable = true;
  };

  services.home-assistant.config = let
    tasmota_s20 = name: topic: {
      platform = "mqtt";
      inherit name;
      state_topic = "stat/${topic}/POWER";
      command_topic = "cmnd/${topic}/POWER";
      payload_on = "ON";
      payload_off = "OFF";
    };
  in {
    homeassistant = {
      name = "Home";
      time_zone = "Europe/Berlin";
      latitude = "52.46187";
      longitude = "13.41489";
      elevation = 90;
      unit_system = "metric";
      customize = friendly_names;
    };
    config = {};
    sun.elevation = 66;
    discovery = {};
    frontend = {};
    mqtt = {
      broker = "localhost";
      port = 1883;
      client_id = "home-assistant";
      username = "gg23";
      password = "gg23-mqtt";
      keepalive = 60;
      protocol = 3.1;

      discovery = true;
      birth_message = {
        topic = "/hass/status";
        payload = "online";
      };
      will_message = {
        topic = "/hass/status";
        payload = "offline";
      };
    };
    sensor = [
      {
        platform = "dwd_weather_warnings";
        region_name = "Berlin";
      }
    ];
    switch = [
      (tasmota_s20 "TV" "tv")
      (tasmota_s20 "Drucker Strom" "drucker")
      (tasmota_s20 "Waschmaschine" "wasch")
      (tasmota_s20 "Stereo Anlage" "stereo")
    ];
    mobile_app = {};
    hue = {};
    weather = [
      {
        platform = "openweathermap";
        api_key = "xxx"; # TODO put into secrets
      }
    ];
    system_health = {};
    history = {};
    shopping_list = {};
  };

  services.mosquitto = {
    enable = true;
    host = "0.0.0.0";
    allowAnonymous = false;
    checkPasswords = true;
    users.gg23 = {
      password = "gg23-mqtt";
      acl = [ "topic readwrite #" ];
    };
  };

  environment.systemPackages = [ pkgs.mosquitto ];
}
