{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

{
  krebs.Reaktor.retiolum = {
    nickname = "Reaktor|lass";
    channels = [ "#retiolum" ];
    extraEnviron = {
      REAKTOR_HOST = "ni.r";
    };
    plugins = with pkgs.ReaktorPlugins; [
      sed-plugin
    ];
  };
}
