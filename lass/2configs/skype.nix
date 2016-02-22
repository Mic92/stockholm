{ config, lib, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;
  inherit (config.krebs.lib) genid;

in {
  users.extraUsers = {
    skype = {
      name = "skype";
      uid = genid "skype";
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
