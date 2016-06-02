{ config, lib, pkgs, ... }:

let
  inherit (config.krebs.lib) genid;

  mainUser = config.users.extraUsers.mainUser;
  createChromiumUser = name: extraGroups: packages:
    {
      users.extraUsers.${name} = {
        inherit name;
        inherit extraGroups;
        home = "/home/${name}";
        uid = genid name;
        useDefaultShell = true;
        createHome = true;
      };
      krebs.per-user.${name}.packages = packages;
      security.sudo.extraConfig = ''
        ${mainUser.name} ALL=(${name}) NOPASSWD: ALL
      '';
      environment.systemPackages = [
        (pkgs.writeScriptBin name ''
          /var/setuid-wrappers/sudo -u ${name} -i chromium $@
        '')
      ];
    };

  createFirefoxUser = name: extraGroups: packages:
    {
      users.extraUsers.${name} = {
        inherit name;
        inherit extraGroups;
        home = "/home/${name}";
        uid = genid name;
        useDefaultShell = true;
        createHome = true;
      };
      krebs.per-user.${name}.packages = packages;
      security.sudo.extraConfig = ''
        ${mainUser.name} ALL=(${name}) NOPASSWD: ALL
      '';
      environment.systemPackages = [
        (pkgs.writeScriptBin name ''
          /var/setuid-wrappers/sudo -u ${name} -i firefox $@
        '')
      ];
    };

  #TODO: abstract this

in {

  environment.systemPackages = [
    (pkgs.writeScriptBin "browser-select" ''
      BROWSER=$(echo -e "ff\ncr\nwk\nfb\ngm\nflash" | dmenu)
      $BROWSER $@
    '')
  ];

  imports = [
    ( createFirefoxUser "ff" [ "audio" ] [ pkgs.firefox ] )
    ( createChromiumUser "cr" [ "video" "audio" ] [ pkgs.chromium ] )
    ( createChromiumUser "wk" [ "video" "audio" ] [ pkgs.chromium ] )
    ( createChromiumUser "fb" [ "video" "audio" ] [ pkgs.chromium ] )
    ( createChromiumUser "gm" [ "video" "audio" ] [ pkgs.chromium ] )
    ( createChromiumUser "com" [ "video" "audio" ] [ pkgs.chromium ] )
  ];
}
