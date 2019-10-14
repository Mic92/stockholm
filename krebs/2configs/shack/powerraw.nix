{ config, lib, pkgs, ... }:
# Replacement for powerraw.shack pollin box
# Requires usb-serial device on host
# Requires mqtt available at mqtt.shack
# Requires hostname powerraw.shack
let
  influx-url = "http://influx.shack:8086";
  pkg = pkgs.python3.pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/powermeter.git";
      rev = "96609f0d632e0732afa768ddd7b3f8841ca37c1b";
      sha256 = "sha256:0wfpm3ik5r081qv2crmpjwylgg2v8ximq347qh0fzq1rwv0dqbnn";
    }) {};
in {
  # receive response from light.shack / standby.shack
  networking.firewall.allowedUDPPorts = [ 11111 ];
  users.users.powermeter.extraGroups = [ "dialout" ];

  systemd.services.powermeter-serial2mqtt = {
    description = "powerraw Serial -> mqtt";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "powermeter";
      ExecStart = "${pkg}/bin/powermeter-serial2mqtt";
      PrivateTmp = true;
      Restart = "always";
      RestartSec = "15";
    };
  };

  systemd.services.powermeter-mqtt2socket = {
    description = "powerraw mqtt -> raw socket 11111";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "powermeter";
      ExecStart = "${pkg}/bin/powermeter-mqtt2socket";
      PrivateTmp = true;
      Restart = "always";
      RestartSec = "15";
    };
  };

  services.telegraf = {
    enable = true;
    extraConfig = {
      agent.debug = false;
      outputs = {
        influxdb = [{
          urls = [ influx-url ];
          database = "telegraf";
        }];
      };
    };
  };

  services.telegraf.extraConfig.inputs.mqtt_consumer = let
    genTopic = name: topic: tags: {
      servers = [ "tcp://mqtt.shack:1883" ];
      qos = 0;
      connection_timeout = "30s";
      topics = [ topic ];
      inherit tags;
      persistent_session = false;
      name_override = name;
      data_format = "value";
      data_type = "float";
    };
    sensor = "total";
    types  = [ "Voltage" "Current" "Power" ];
    phases = [ 1 2 3 ];
  in
    [ (genTopic "Power consumed" "/power/${sensor}/consumed"  { inherit sensor; }) ] ++
    (lib.flatten (map (type: (map (phase: (genTopic "Power" "/power/${sensor}/L${toString phase}/${type}" { inherit sensor phase type; }) ) phases)) types));
}
