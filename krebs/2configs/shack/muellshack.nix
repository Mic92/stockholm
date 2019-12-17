{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/muellshack";
      rev = "c3d1f70325e5b90f280c5be60110e14f4de653ae";
      sha256 = "1dd4kqwdr4v413rmkvmyjzzvw8id9747nifp96pg0c2cy6bhzj24";
    }) { mkYarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage; };
    home = "/var/lib/muellshack";
    port = "8081";
in {
  users.users.muellshack = {
    inherit home;
    createHome = true;
  };
  services.nginx.virtualHosts."muell.shack" = {
    locations."/" = {
      proxyPass = "http://localhost:${port}/muellshack/";
    };
  };
  services.nginx.virtualHosts."openhab.shack" = {
    locations."/muellshack/".proxyPass = "http://localhost:${port}/muellshack/";
  };
  systemd.services.muellshack = {
    description = "muellshack";
    wantedBy = [ "multi-user.target" ];
    environment.PORT = port;
    serviceConfig = {
      User = "muellshack";
      # do not override the current storage fil
      ExecStartPre = pkgs.writeDash "call-muell-pre" ''
        cp -vf ${pkg}/share/static_muelldata.json ${home}
        cp -vn  ${pkg}/share/storage.json ${home}
        chmod 700 ${home}/storage.json
      '';
      WorkingDirectory = home;
      ExecStart = "${pkg}/bin/muellshack";
      PrivateTmp = true;
      Restart = "always";
      RestartSec = "15";
    };
  };
}
