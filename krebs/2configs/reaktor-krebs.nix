{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

{
  krebs.Reaktor.krebs = {
    nickname = "Reaktor|krebs";
    channels = [
      "#krebs"
      "#nixos-wiki"
    ];
    extraEnviron = {
      REAKTOR_HOST = "irc.freenode.org";
    };
    plugins = with pkgs.ReaktorPlugins; [
      sed-plugin
    ] ++
      (attrValues (todo "agenda"))
    ;
  };
}
