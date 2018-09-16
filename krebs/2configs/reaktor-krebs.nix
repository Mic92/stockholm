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
      REAKTOR_NICKSERV_PASSWORD = "/var/lib/Reaktor/reaktor_nickserv_password";
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
  krebs.secret.files.nix-serve-key = {
    path = "/var/lib/Reaktor/reaktor_nickserv_password";
    owner.name = "Reaktor";
    source-path = toString <secrets> + "/reaktor_nickserv_password";
  };
}
