{ config, lib, pkgs, ... }:
{
  services.snapserver = {
    enable = true;
    openFirewall = true;
      streams = {
      pipewire  = {
        type = "pipe";
        location = "/run/snapserver/snapfifo";
      };
    };
  };
}
