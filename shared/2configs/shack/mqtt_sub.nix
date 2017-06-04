{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  pkg = pkgs.stdenv.mkDerivation {
    name = "mqtt2graphite-2017-05-29";
    src = pkgs.fetchgit {
      url = "https://github.com/shackspace/mqtt2graphite/";
      rev = "8c060e6";
      sha256 = "06x7a1j6sfyvvdxg0366fcslhn478anqh4m5hljyf0z29knvz7pg";
    };
    buildInputs = [
      (pkgs.python35.withPackages (pythonPackages: with pythonPackages; [
        docopt
        paho-mqtt
      ]))
    ];
    installPhase = ''
      install -m755 -D sub.py  $out/bin/sub
      install -m755 -D sub2.py  $out/bin/sub-new
    '';
  };
in {
  systemd.services.mqtt_sub  = {
    description = "subscribe to mqtt, send to graphite";
    # after = [ (lib.optional config.services.mosqitto.enable "mosquitto.service") ];
    wantedBy = [ "multi-user.target"  ];
    serviceConfig = {
      User = "nobody";
      ExecStart = "${pkg}/bin/sub-new";
      PrivateTmp = true;
    };
  };
}
