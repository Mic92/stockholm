{ config, lib, pkgs, ... }:

with import ../../../lib/pure.nix { inherit lib; };
let
  pkg = pkgs.stdenv.mkDerivation {
    name = "mqtt2graphite-2017-05-29";
    src = pkgs.fetchgit {
      url = "https://github.com/shackspace/mqtt2graphite/";
      rev = "117179d";
      sha256 = "1334jbbzlqizyp7zcn4hdswhhrnkj1p4p435n5nph82lzffrsi44";
    };
    buildInputs = [
      (pkgs.python3.withPackages (pythonPackages: with pythonPackages; [
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
      Restart = "always";
      RestartSec = "15";
    };
  };
}
