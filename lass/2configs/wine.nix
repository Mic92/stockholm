{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;

in {
  krebs.per-user.wine.packages = with pkgs; [
    wineUnstable
  ];
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
    };
  };
  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(wine) NOPASSWD: ALL
  '';
}
