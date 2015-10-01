{ config, lib, pkgs, ... }:

let
  inherit (import ../4lib { inherit pkgs lib; }) simpleScript;

  mainUser = config.users.extraUsers.mainUser;
  createChromiumUser = name: extraGroups: packages:
    {
      users.extraUsers = {
        ${name} = {
          inherit name;
          inherit extraGroups;
          home = "/home/${name}";
          useDefaultShell = true;
          createHome = true;
        };
      };
      lass.per-user.${name}.packages = packages;
      security.sudo.extraConfig = ''
        ${mainUser.name} ALL=(${name}) NOPASSWD: ALL
      '';
      environment.systemPackages = [
        (simpleScript name ''
          sudo -u ${name} -i chromium $@
        '')
      ];
    };

  createFirefoxUser = name: extraGroups: packages:
    {
      users.extraUsers = {
        ${name} = {
          inherit name;
          inherit extraGroups;
          home = "/home/${name}";
          useDefaultShell = true;
          createHome = true;
        };
      };
      lass.per-user.${name}.packages = packages;
      security.sudo.extraConfig = ''
        ${mainUser.name} ALL=(${name}) NOPASSWD: ALL
      '';
      environment.systemPackages = [
        (simpleScript name ''
          sudo -u ${name} -i firefox $@
        '')
      ];
    };

  #TODO: abstract this

in {

  environment.systemPackages = [
    (simpleScript "browser-select" ''
      BROWSER=$(echo -e "ff\ncr\nfb\ngm\nflash" | dmenu)
      $BROWSER $@
    '')
  ];

  imports = [
    ../3modules/per-user.nix
  ] ++ [
    ( createFirefoxUser "ff" [ "audio" ] [ ] )
    ( createChromiumUser "cr" [ "audio" ] [ pkgs.chromium ] )
    ( createChromiumUser "fb" [ ] [ pkgs.chromium ] )
    ( createChromiumUser "gm" [ ] [ pkgs.chromium ] )
   # ( createChromiumUser "flash" [ ] [ pkgs.flash ] )
  ];

  nixpkgs.config.packageOverrides = pkgs : {
    flash = pkgs.chromium.override {
    #  pulseSupport = true;
      enablePepperFlash = true;
    };
    #chromium = pkgs.chromium.override {
    #  pulseSupport = true;
    #};
  };
}
