{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;

in {
  environment.systemPackages = with pkgs; [
    wineUnstable
  ];
  users.users= {
    wine = {
      name = "wine";
      description = "user for running wine";
      home = "/home/wine";
      useDefaultShell = true;
      extraGroups = [ "audio" ];
      createHome = true;
    };
  };
  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(wine) NOPASSWD: ALL
  '';
}
