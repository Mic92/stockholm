{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;
  vdoom = pkgs.writeDash "vdoom" ''
    ${pkgs.zandronum-bin}/bin/zandronum \
      -fov 120 \
      "$@"
  '';
  doom = pkgs.writeDash "doom" ''
    DOOM_DIR=''${DOOM_DIR:-~/doom/}
    ${vdoom} \
      -file $DOOM_DIR/lib/brutalv20.pk3 \
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
  vdoom1 = pkgs.writeDashBin "vdoom1" ''
    DOOM_DIR=''${DOOM_DIR:-~/doom/}
    ${vdoom} -iwad $DOOM_DIR/wads/stock/doom.wad "$@"
  '';
  vdoom2 = pkgs.writeDashBin "vdoom2" ''
    DOOM_DIR=''${DOOM_DIR:-~/doom/}
    ${vdoom} -iwad $DOOM_DIR/wads/stock/doom2.wad "$@"
  '';

  doomservercfg = pkgs.writeText "doomserver.cfg" ''
    skill 7
    #survival true
    #sv_maxlives 4
    #sv_norespawn true
    #sv_weapondrop true
    no_jump true
    #sv_noweaponspawn true
    sv_sharekeys true
    sv_survivalcountdowntime 1
    sv_noteamselect true
    sv_updatemaster false
    #sv_coop_loseinventory true
    #cl_startasspectator false
    #lms_spectatorview false
  '';

  vdoomserver = pkgs.writeDashBin "vdoomserver" ''
    DOOM_DIR=''${DOOM_DIR:-~/doom/}

    ${pkgs.zandronum-bin}/bin/zandronum-server \
    +exec ${doomservercfg} \
    "$@"
  '';

in {
  environment.systemPackages = with pkgs; [
    dwarf_fortress
    doom1
    doom2
    vdoom1
    vdoom2
    vdoomserver
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

  hardware.pulseaudio.support32Bit = true;

  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(games) NOPASSWD: ALL
  '';
}
