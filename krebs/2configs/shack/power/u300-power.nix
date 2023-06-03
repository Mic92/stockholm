{ pkgs, ... }:
let
  src = pkgs.fetchFromGitHub {
    repo = "shackstrom";
    owner = "samularity";
    rev = "adfbdc7d12000fbc9fd9367c8ef0a53b7d0a9fad";
    hash = "sha256-77vSX2+1XXaBVgLka+tSEK/XYZASEk9iq+uEuO1aOUQ=";
  };
  pkg = pkgs.writers.writePython3 "test_python3" {
    libraries = [ pkgs.python3Packages.requests pkgs.python3Packages.paho-mqtt  ];
  } (builtins.readFile "${src}/shackstrom.py");
in
{
    systemd.services = {
      u300-power = {
        enable = true;
        environment = { 
          DATA_URL = "http://10.42.20.255/csv.html";
          BROKER = "mqtt.shack";
        };
        serviceConfig = {
          Restart = "always";
          ExecStart = pkg;
          RestartSec = "15s";
        };
        wantedBy = [ "multi-user.target" ];
      };
    };
}
