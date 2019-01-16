{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

{
  krebs.Reaktor.retiolum = {
    nickname = "Reaktor|lass";
    channels = [ "#noise" "#xxx" ];
    extraEnviron = {
      REAKTOR_HOST = "irc.r";
    };
    plugins = with pkgs.ReaktorPlugins; [
      sed-plugin
    ] ++
      (attrValues (task "agenda"))
    ;
  };
}
