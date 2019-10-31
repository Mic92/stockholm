{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/node-light.git";
      rev = "32d8064db5172b8068f633211c8bd5688b2c8773";
      sha256 = "14jzhs7pp3hq42wq3cwqarivn1z7vcgksfzfqfc4yyh21096yi1j";
    }) { mkYarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage; };
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
