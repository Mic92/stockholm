{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;

in {
  users.users= {
    wine = {
      name = "wine";
      description = "user for running wine";
      home = "/home/wine";
      useDefaultShell = true;
      extraGroups = [
        "audio"
        "video"
      ];
      createHome = true;
      packages = [
        pkgs.wine
      ];
    };
    wine64 = {
      name = "wine64";
      description = "user for running wine in 64bit";
      home = "/home/wine64";
      useDefaultShell = true;
      extraGroups = [
        "audio"
        "video"
      ];
      createHome = true;
      packages = [
        (pkgs.wine.override { wineBuild = "wineWow"; })
      ];
    };
  };
  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(wine) NOPASSWD: ALL
    ${mainUser.name} ALL=(wine64) NOPASSWD: ALL
  '';
}
