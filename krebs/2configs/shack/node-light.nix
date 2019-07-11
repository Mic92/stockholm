{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/node-light.git";
      rev = "a32c782650c4cc0adf51250fe249167d7246c59b";
      sha256 = "0clvcp1m2ay0a9ibh7s21q7d9a6nam3497bysvc6mdygblks22qy";
    }) {};
    home = "/var/lib/node-light";
    port = "8082";
in {
  # receive response from light.shack / standby.shack
  networking.firewall.allowedUDPPorts = [ 2342 ];
  users.users.node-light = {
    inherit home;
    createHome = true;
  };
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
    serverAliases = [ "lightapi.shack" ];
    locations."/power".proxyPass = "http://localhost:${port}";
    locations."/lounge".proxyPass = "http://localhost:${port}";
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
      Restart = "always";
      PrivateTmp = true;
    };
  };
}
