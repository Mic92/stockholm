{ config, lib, pkgs, ... }:
with import <stockholm/lib>;
let

  cfg = config.lass.autowifi;

in {
  options.lass.autowifi = {
    enable = mkEnableOption "automatic wifi connector";
    knownWifisFile = mkOption {
      type = types.str;
      default = "/etc/wifis";
    };
  };

  config = {
    systemd.services.autowifi = {
      description = "Automatic wifi connector";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "10s";
        ExecStart = pkgs.writers.writePython3 "autowifi" {} /* python3 */ ''
          import subprocess
          import time
          import urllib.request


          def connect(ssid, psk=None):
              subprocess.run(["${pkgs.networkmanager}/bin/nmcli", "connection", "delete", "autowifi"])
              print("connecting to {}".format(ssid))
              if psk is None:
                  subprocess.run(["${pkgs.networkmanager}/bin/nmcli", "device", "wifi", "connect", ssid, "name", "autowifi"])
              else:
                  subprocess.run(["${pkgs.networkmanager}/bin/nmcli", "device", "wifi", "connect", ssid, "name", "autowifi", "password", psk])


          def scan():
              wifis_raw = subprocess.check_output(["${pkgs.networkmanager}/bin/nmcli", "-t", "device", "wifi", "list", "--rescan", "yes"])
              wifis_list = wifis_raw.split(b'\n')
              wifis = []
              for line in wifis_list:
                  ls = line.split(b':')
                  if len(ls) == 8:
                      wifis.append({"ssid": ls[1], "signal": int(ls[5]), "crypto": ls[7]})
              return wifis


          def get_known_wifis():
              wifis_lines = []
              with open('${cfg.knownWifisFile}') as f:
                  wifis_lines = f.read().splitlines()
              wifis = []
              for line in wifis_lines:
                  ls = line.split(':')
                  wifis.append({"ssid": ls[0].encode(), "psk": ls[1].encode()})
              return wifis


          def check_internet():
              try:
                  beacon = urllib.request.urlopen('http://krebsco.de/secret')
              except:  # noqa
                  print("no internet")
                  return False
              if beacon.read() == b'1337\n':
                  return True
              print("no internet")
              return False


          def is_wifi_open(wifi):
              if wifi['crypto'] == ${"b''"}:
                  return True
              else:
                  return False


          def is_wifi_seen(wifi, seen_wifis):
              for seen_wifi in seen_wifis:
                  if seen_wifi["ssid"] == wifi["ssid"]:
                      return True
              return False


          def bloop():
              while True:
                  if not check_internet():
                      wifis = scan()
                      known_wifis = get_known_wifis()
                      known_seen_wifis = [wifi for wifi in known_wifis if is_wifi_seen(wifi, wifis)]
                      for wifi in known_seen_wifis:
                          connect(wifi['ssid'], wifi['psk'])
                          if check_internet():
                              continue
                      open_wifis = filter(is_wifi_open, wifis)
                      for wifi in open_wifis:
                          connect(wifi['ssid'])
                          if check_internet():
                              continue
                  time.sleep(10)


          bloop()
        '';
      };
    };
  };
}

