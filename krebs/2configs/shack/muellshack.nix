{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchgit {
      url = "https://git.shackspace.de/rz/muellshack";
      rev = "d8a5e2d4c0a22804838675ac42b468299dcd9a76";
      sha256 = "0ff6q64dgdxmpszp94z100fdic175b1vvxn4crg8p0jcabzxsv0m";
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
