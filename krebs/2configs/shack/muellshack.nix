{ config, lib, pkgs, ... }:

let
  pkg = pkgs.callPackage (
    pkgs.fetchFromGitHub {
      owner = "shackspace";
      repo = "muellshack";
      rev = "dc80cf1edaa3d86ec2bebae8596ad1d4c4e3650a";
      sha256 = "1yipr66zhrg5m20pf3rzvgvvl78an6ddkq6zc45rxb2r0i7ipkyh";

    }) { mkYarnPackage = pkgs.yarn2nix-moretea.mkYarnPackage; };
    home = "/var/lib/muellshack";
    port = "8081";
in {
  users.users.muellshack = {
    inherit home;
    isSystemUser = true;
    createHome = true;
    group = "muellshack";
  };
  users.groups.muellshack = {};
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
