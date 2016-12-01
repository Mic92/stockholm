{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;
  doom = pkgs.writeDash "doom" ''
    DOOM_DIR=''${DOOM_DIR:-~/doom/}
    ${pkgs.zandronum}/bin/zandronum \
      -file $DOOM_DIR/lib/brutalv20.pk3 \
      -file $DOOM_DIR/lib/RebotStarcraftMarines.pk3 \
      -fov 120 \
      "$@"
  '';
  doom1 = pkgs.writeDashBin "doom1" ''
    DOOM_DIR=''${DOOM_DIR:-~/doom/}
    ${doom} -iwad $DOOM_DIR/wads/stock/doom.wad "$@"
  '';
  doom2 = pkgs.writeDashBin "doom2" ''
    DOOM_DIR=''${DOOM_DIR:-~/doom/}
    ${doom} -iwad $DOOM_DIR/wads/stock/doom2.wad "$@"
  '';

in {
  environment.systemPackages = with pkgs; [
    dwarf_fortress
    doom1
    doom2
  ];

  users.extraUsers = {
    games = {
      name = "games";
      description = "user playing games";
      home = "/home/games";
      extraGroups = [ "audio" "video" "input" "loot" ];
      createHome = true;
      useDefaultShell = true;
    };
  };

  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(games) NOPASSWD: ALL
  '';
}
