{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

{
  krebs.Reaktor.retiolum = {
    nickname = "Reaktor|lass";
    channels = [ "#xxx" ];
    extraEnviron = {
      REAKTOR_HOST = "irc.r";
    };
    plugins = with pkgs.ReaktorPlugins; [
      sed-plugin
    ];
  };
}
