{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let

  mainUser = config.users.extraUsers.mainUser;

  browser-select = pkgs.writeScriptBin "browser-select" ''
    BROWSER=$(echo -e "${concatStringsSep "\\n" (attrNames config.lass.browser.paths)}" | ${pkgs.dmenu}/bin/dmenu)
    case $BROWSER in
    ${concatMapStringsSep "\n" (n: ''
      ${n})
        export BIN=${config.lass.browser.paths.${n}}/bin/${n}
        ;;
    '') (attrNames config.lass.browser.paths)}
    esac
    $BIN "$@"
  '';

  createChromiumUser = name: extraGroups:
    let
      bin = pkgs.writeScriptBin name ''
        /var/run/wrappers/bin/sudo -u ${name} -i ${pkgs.chromium}/bin/chromium $@
      '';
    in {
      users.extraUsers.${name} = {
        inherit name;
        inherit extraGroups;
        home = "/home/${name}";
        uid = genid name;
        useDefaultShell = true;
        createHome = true;
      };
      lass.browser.paths.${name} = bin;
      security.sudo.extraConfig = ''
        ${mainUser.name} ALL=(${name}) NOPASSWD: ALL
      '';
      environment.systemPackages = [
        bin
      ];
    };

  createFirefoxUser = name: extraGroups:
    let
      bin = pkgs.writeScriptBin name ''
        /var/run/wrappers/bin/sudo -u ${name} -i ${pkgs.firefox}/bin/firefox $@
      '';
    in {
      users.extraUsers.${name} = {
        inherit name;
        inherit extraGroups;
        home = "/home/${name}";
        uid = genid name;
        useDefaultShell = true;
        createHome = true;
      };
      lass.browser.paths.${name} = bin;
      security.sudo.extraConfig = ''
        ${mainUser.name} ALL=(${name}) NOPASSWD: ALL
      '';
      environment.systemPackages = [
        bin
      ];
    };

  #TODO: abstract this

in {

  lass.browser.select = browser-select;

  environment.systemPackages = [
    browser-select
  ];

  imports = [
    {
      options.lass.browser.select = mkOption {
        type = types.path;
      };
      options.lass.browser.paths = mkOption {
        type = with types; attrsOf path;
      };
    }
    ( createFirefoxUser "ff" [ "audio" ] )
    ( createChromiumUser "cr" [ "video" "audio" ] )
    ( createChromiumUser "wk" [ "video" "audio" ] )
    ( createChromiumUser "fb" [ "video" "audio" ] )
    ( createChromiumUser "gm" [ "video" "audio" ] )
    ( createChromiumUser "com" [ "video" "audio" ] )
  ];
}
