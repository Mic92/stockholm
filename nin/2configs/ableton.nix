{ config, pkgs, ... }: let
  mainUser = config.users.extraUsers.nin;
in {
  users.users= {
    ableton = {
      isNormalUser = true;
      extraGroups = [
        "audio"
        "video"
      ];
      packages = [
        pkgs.wine
        pkgs.winetricks
      ];
    };
  };
  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(ableton) NOPASSWD: ALL
  '';
}
