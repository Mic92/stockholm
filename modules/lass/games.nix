{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;

in {
  environment.systemPackages = with pkgs; [
    dwarf_fortress
  ];

  users.extraUsers = {
    games = {
      name = "games";
      description = "user playing games";
      home = "/home/games";
      extraGroups = [ "audio" "video" ];
      createHome = true;
      useDefaultShell = true;
    };
  };

  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(games) NOPASSWD: ALL
  '';
}
