{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchFromGitHub {
      owner = "shackspace";
      repo = "node-light";
      rev = "90a9347b73af3a9960bd992e6293b357226ef6a0";
      sha256 = "1av9w3w8aknlra25jw6gqxzbb01i9kdlfziy29lwz7mnryjayvwk";
    }) { };
    home = "/var/lib/node-light";
    port = "8082";
in {
  # receive response from light.shack / standby.shack
  networking.firewall.allowedUDPPorts = [ 2342 ];
  users.users.node-light = {
    inherit home;
    isSystemUser = true;
    createHome = true;
    group = "node-light";
  };
  users.groups.node-light = {};
  services.nginx.virtualHosts."lounge.light.shack" = {
    locations."/" = {
      proxyPass = "http://localhost:${port}/lounge/";
    };
  };
  services.nginx.virtualHosts."power.light.shack" = {
    locations."/" = {
      proxyPass = "http://localhost:${port}/power/";
    };
  };

  services.nginx.virtualHosts."openhab.shack" = {
    extraConfig = ''
      access_log syslog:server=unix:/dev/log combined if=$loggable;
    '';
    serverAliases = [ "lightapi.shack" ];
    locations."/power/".proxyPass = "http://localhost:${port}/power/";
    locations."/lounge/".proxyPass = "http://localhost:${port}/lounge/";
  };
  systemd.services.node-light= {
    description = "node-light";
    wantedBy = [ "multi-user.target" ];
    environment.PORT = port;
    serviceConfig = {
      User = "node-light";
      # do not override the current storage file
      ExecStartPre = pkgs.writeDash "call-light-pre" ''
        cp -vn  ${pkg}/share/storage.json ${home}
        chmod 700 ${home}/storage.json

      '';
      WorkingDirectory = home;
      ExecStart = "${pkg}/bin/node-light";
      PrivateTmp = true;
      Restart = "always";
      RestartSec = "15";
    };
  };
}
