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
      wiki-todo-add
      wiki-todo-done
      wiki-todo-show
    ];
  };
  services.nginx.virtualHosts."lassul.us".locations."/wiki-todo".extraConfig = ''
    default_type "text/plain";
    alias /var/lib/Reaktor/state/wiki-todo;
  '';
}
