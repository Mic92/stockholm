{ config, pkgs, ... }:

{
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
    lass ALL=(elster) NOPASSWD: ALL
  '';
}
