{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;

in {
  users.extraUsers = {
    skype = {
      name = "skype";
      uid = 2259819492; #genid skype
      description = "user for running skype";
      home = "/home/skype";
      useDefaultShell = true;
      extraGroups = [ "audio" "video" ];
      createHome = true;
    };
  };

  krebs.per-user.skype.packages = [
    pkgs.skype
  ];

  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(skype) NOPASSWD: ALL
  '';
}
