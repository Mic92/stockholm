{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let

  mainUser = config.users.extraUsers.mainUser;

  browser-select = let
    sortedPaths = sort (a: b: a.value.precedence > b.value.precedence)
                       (mapAttrsToList (name: value: { inherit name value; })
                                       config.lass.browser.paths);
  in pkgs.writeScriptBin "browser-select" ''
    BROWSER=$(echo -e "${concatStringsSep "\\n" (map (getAttr "name") sortedPaths)}" | ${pkgs.dmenu}/bin/dmenu)
    case $BROWSER in
    ${concatMapStringsSep "\n" (n: ''
      ${n.name})
        export BIN=${n.value.path}/bin/${n.name}
        ;;
    '') (sortedPaths)}
    esac
    $BIN "$@"
  '';

  createChromiumUser = name: groups: precedence:
    {
      lass.xjail.${name} = {
        user = name;
        script = pkgs.writeDash name ''
          ${pkgs.chromium}/bin/chromium "$@"
        '';
        inherit groups;
      };
      environment.systemPackages = [ config.lass.xjail-bins.${name} ];
      lass.browser.paths.${name} = {
        path = config.lass.xjail-bins.${name};
        inherit precedence;
      };
    };

  createFirefoxUser = name: groups: precedence:
    {
      lass.xjail.${name} = {
        user = name;
        script = pkgs.writeDash name ''
          ${pkgs.firefox-devedition-bin}/bin/firefox-devedition "$@"
        '';
        inherit groups;
      };
      environment.systemPackages = [ config.lass.xjail-bins.${name} ];
      lass.browser.paths.${name} = {
        path = config.lass.xjail-bins.${name};
        inherit precedence;
      };
    };

  createQuteUser = name: groups: precedence:
    {
      lass.xjail.${name} = {
        user = name;
        script = pkgs.writeDash name ''
          ${pkgs.qutebrowser}/bin/qutebrowser "$@"
        '';
        inherit groups;
      };
      environment.systemPackages = [ config.lass.xjail-bins.${name} ];
      lass.browser.paths.${name} = {
        path = config.lass.xjail-bins.${name};
        inherit precedence;
      };
    };


  #TODO: abstract this

in {

  lass.browser.select = browser-select;

  environment.systemPackages = [
    browser-select
  ];

  programs.chromium = {
    enable = true;
    extensions = [
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
      "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
    ];
  };

  imports = [
    {
      options.lass.browser.select = mkOption {
        type = types.path;
      };
      options.lass.browser.paths = mkOption {
        type = types.attrsOf (types.submodule ({
          options = {
            path = mkOption {
              type = types.path;
            };
            precedence = mkOption {
              type = types.int;
              default = 0;
            };
          };
        }));
      };
    }
    ( createFirefoxUser "ff" [ "audio" ] 10 )
    ( createChromiumUser "cr" [ "audio" ] 9 )
    ( createChromiumUser "gm" [ "video" "audio" ] 8 )
    ( createChromiumUser "wk" [ "audio" ] 0 )
    ( createChromiumUser "fb" [ "audio" ] 0 )
    ( createChromiumUser "com" [ "audio" ] 0 )
    ( createChromiumUser "fin" [] (-1) )
  ];
}
