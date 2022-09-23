{ config, lib, pkgs, ... }:
{
  systemd.services.snapclient = {
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.snapcast ];
    script = "snapclient -h 10.42.0.1";
    serviceConfig = {
      DynamicUser = true;
      Group = "pipewire";
    };
  };
}
