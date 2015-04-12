{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;

in {
  imports = [
    ../tv/xserver.nix
  ];
  services.xserver.displayManager.auto.user = mainUser.name;
}
