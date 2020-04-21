{ config, lib, pkgs, ... }:
{
  imports = [
    ./zigbee.nix
    ./rooms/bett.nix
  ];

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i int0 -p tcp --dport 1883"; target = "ACCEPT"; } # mosquitto
    { predicate = "-i docker0 -p tcp --dport 1883"; target = "ACCEPT"; } # mosquitto
    { predicate = "-i int0 -p tcp --dport 8123"; target = "ACCEPT"; } # hass
    { predicate = "-i retiolum -p tcp --dport 8123"; target = "ACCEPT"; } # hass
  ];

  services.home-assistant = {
    enable = true;
    package = pkgs.home-assistant.override {
      # extraComponents = [ "hue" ];
    };
    configWritable = true;
  };

  lass.hass.config = let
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
      latitude = "48.7687";
      longitude = "9.2478";
      elevation = 247;
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
    ];
    switch = [
      (tasmota_s20 "TV" "tv")
      (tasmota_s20 "Drucker Strom" "drucker")
      (tasmota_s20 "Waschmaschine" "wasch")
      (tasmota_s20 "Stereo Anlage" "stereo")
    ];
    mobile_app = {};
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
