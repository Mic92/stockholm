{ config, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/mouse.nix>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/wine.nix>
    <stockholm/lass/2configs/bitcoin.nix>
    <stockholm/lass/2configs/backup.nix>
    <stockholm/lass/2configs/blue-host.nix>
    <stockholm/lass/2configs/green-host.nix>
    <stockholm/lass/2configs/ssh-cryptsetup.nix>
    <stockholm/lass/2configs/nfs-dl.nix>
  ];

  krebs.build.host = config.krebs.hosts.shodan;

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';

  #media center
  users.users.media = {
    isNormalUser = true;
    uid = genid_uint31 "media";
    extraGroups = [ "video" "audio" ];
  };

  services.xserver.displayManager.lightdm.autoLogin = {
    enable = true;
    user = "media";
  };

  #hass
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 8123"; target = "ACCEPT"; }
    { predicate = "-p tcp --dport 1883"; target = "ACCEPT"; }
    # zerotierone
    { predicate = "-p udp --dport 9993"; target = "ACCEPT"; }
  ];

  services.home-assistant = let
    tasmota_s20 = name: topic: {
      platform = "mqtt";
      inherit name;
      state_topic = "stat/${topic}/POWER";
      command_topic = "cmnd/${topic}/POWER";
      payload_on = "ON";
      payload_off = "OFF";
    };
  in {
    enable = true;
    package = pkgs.home-assistant.override {
      python3 = pkgs.python36;
      #extraComponents = [
      #  (pkgs.fetchgit {
      #    url = "https://github.com/marcschumacher/dwd_pollen";
      #    rev = "0.1";
      #    sha256 = "12vldwsds27c9l15ffc6svk9mj17jhypcz736pvpmpqbsymllz2p";
      #  })
      #];
    };
    config = {
      homeassistant = {
        name = "Home"; time_zone = "Europe/Berlin";
        latitude = "48.7687";
        longitude = "9.2478";
        elevation = 247;
      };
      sun.elevation = 66;
      discovery = {};
      frontend = { };
      mqtt = {
        broker = "localhost";
        port = 1883;
        client_id = "home-assistant";
        username = "gg23";
        password = "gg23-mqtt";
        keepalive = 60;
        protocol = 3.1;
      };
      sensor = [
      ];
      switch = [
        (tasmota_s20 "Drucker Strom" "drucker")
        (tasmota_s20 "Bett Licht" "bett")
      ];
      device_tracker = [
        {
          platform = "luci";
        }
      ];
    };
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
