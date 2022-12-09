with import ./lib;
{ config, pkgs, ... }: let
  cfg.user = config.krebs.build.user;
in {
  tv.Xresources = {
    "Sxiv.foreground" = "#232323";
    "Sxiv.background" = "#424242";
  };
  users.users.${cfg.user.name}.packages = [
    pkgs.sxiv
  ];
}
