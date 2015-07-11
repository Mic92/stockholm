{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;

in {
  users.extraUsers = {
    elster = {
      name = "elster";
      description = "user for running elster-online";
      home = "/home/elster";
      useDefaultShell = true;
      extraGroups = [];
      createHome = true;
    };
  };
  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(elster) NOPASSWD: ALL
  '';
}
