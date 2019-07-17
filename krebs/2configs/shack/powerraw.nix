{ config, lib, pkgs, ... }:
# Replacement for powerraw.shack pollin box
# Requires usb-serial device on host
# Requires mqtt available at mqtt.shack
# Requires hostname powerraw.shack
let
  pkg = pkgs.python3.pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/powermeter.git";
      rev = "96609f0d632e0732afa768ddd7b3f8841ca37c1b";
      sha256 = "sha256:0wfpm3ik5r081qv2crmpjwylgg2v8ximq347qh0fzq1rwv0dqbnn";
    }) {};
in {
  # receive response from light.shack / standby.shack
  networking.firewall.allowedUDPPorts = [ 11111 ];
  users.users.powermeter = {
    extraGroups = [ "dialout" ];
  };

  systemd.services.powermeter-serial2mqtt = {
    description = "powerraw Serial -> mqtt";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "powermeter";
      ExecStart = "${pkg}/bin/powermeter-serial2mqtt";
      Restart = "always";
      PrivateTmp = true;
    };
  };

  systemd.services.powermeter-mqtt2socket = {
    description = "powerraw mqtt -> raw socket 11111";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "powermeter";
      ExecStart = "${pkg}/bin/powermeter-mqtt2socket";
      Restart = "always";
      PrivateTmp = true;
    };
  };
}
