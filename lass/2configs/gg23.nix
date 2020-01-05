{ config, pkgs, ... }:
with import <stockholm/lib>;

{
  networking.networkmanager.unmanaged = [ "int0" ];
  networking.interfaces.int0.ipv4.addresses = [{
    address = "10.42.0.1";
    prefixLength = 24;
  }];

  services.dhcpd4 = {
    enable = true;
    interfaces = [ "int0" ];
    extraConfig = ''
      option subnet-mask 255.255.255.0;
      option routers 10.42.0.1;
      option domain-name-servers 10.42.0.1;
      subnet 10.42.0.0 netmask 255.255.255.0 {
        range 10.42.0.100 10.42.0.200;
      }
    '';
    machines = [
      { ethernetAddress = "c8:3d:d4:2c:40:ae"; hostName = "tv"; ipAddress = "10.42.0.3"; }
      { ethernetAddress = "3c:2a:f4:22:28:37"; hostName = "drucker"; ipAddress = "10.42.0.4"; }
      { ethernetAddress = "80:7d:3a:67:b7:01"; hostName = "s20-bett"; ipAddress = "10.42.0.10"; }
      { ethernetAddress = "80:7d:3a:68:04:f0"; hostName = "s20-drucker"; ipAddress = "10.42.0.11"; }
      { ethernetAddress = "80:7d:3a:68:11:a5"; hostName = "s20-kueche"; ipAddress = "10.42.0.12"; }
      { ethernetAddress = "80:7d:3a:67:bb:69"; hostName = "s20-stereo"; ipAddress = "10.42.0.13"; }
      { ethernetAddress = "80:8d:b7:c5:80:dc"; hostName = "arubaAP"; ipAddress = "10.42.0.99"; }
    ];
  };

  services.dnsmasq = {
    enable = true;
    resolveLocalQueries = false;

    extraConfig = ''
      local=/gg23/
      domain=gg23
      expand-hosts
      listen-address=10.42.0.1
      interface=int0
    '';
  };

  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i int0 -p tcp --dport 8123"; target = "ACCEPT"; } # hass
    { predicate = "-i retiolum -p tcp --dport 8123"; target = "ACCEPT"; } # hass
    { predicate = "-i int0 -p tcp --dport 1883"; target = "ACCEPT"; } # mosquitto
    { predicate = "-i int0 -p udp --dport 53"; target = "ACCEPT"; } # dns
  ];
  krebs.iptables.tables.filter.FORWARD.rules = [
    { v6 = false; predicate = "-d 10.42.0.0/24 -o int0 -m conntrack --ctstate RELATED,ESTABLISHED"; target = "ACCEPT"; }
    { v6 = false; predicate = "-s 10.42.0.0/24 -i int0"; target = "ACCEPT"; }
    { v6 = false; predicate = "-o int0"; target = "REJECT --reject-with icmp-port-unreachable"; }
    { v6 = false; predicate = "-i int0"; target = "REJECT --reject-with icmp-port-unreachable"; }
  ];
  krebs.iptables.tables.nat.PREROUTING.rules = [
    { v6 = false; predicate = "-s 10.42.0.0/24"; target = "ACCEPT"; precedence = 1000; }
  ];
  krebs.iptables.tables.nat.POSTROUTING.rules = [
    { v6 = false; predicate = "-s 10.42.0.0/24 ! -d 10.42.0.0/24"; target = "MASQUERADE"; }
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
        (tasmota_s20 "Kueche Licht" "kueche")
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

