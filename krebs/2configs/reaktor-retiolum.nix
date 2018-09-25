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
      task-add
      task-delete
      task-done
      task-list
    ] ++
      (attrValues (todo "agenda"))
    ;
  };
}
