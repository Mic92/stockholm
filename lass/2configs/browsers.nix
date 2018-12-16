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

  createUser = script: name: groups: precedence: dpi:
    {
      lass.xjail.${name} = {
        inherit script groups dpi;
      };
      environment.systemPackages = [
        config.lass.xjail-bins.${name}
        (pkgs.writeDashBin "cx-${name}" ''
          DISPLAY=:${toString (genid_uint31 name)} ${pkgs.xclip}/bin/xclip -o | DISPLAY=:0 ${pkgs.xclip}/bin/xclip
        '')
      ];
      lass.browser.paths.${name} = {
        path = config.lass.xjail-bins.${name};
        inherit precedence;
      };
    };

  createChromiumUser = name: groups: precedence:
    createUser (pkgs.writeDash name ''
      ${pkgs.chromium}/bin/chromium "$@"
    '') name groups precedence 80;

  createFirefoxUser = name: groups: precedence:
    createUser (pkgs.writeDash name ''
      ${pkgs.firefox}/bin/firefox "$@"
    '') name groups precedence 80;

  createQuteUser = name: groups: precedence:
    createUser (pkgs.writeDash name ''
      ${pkgs.qutebrowser}/bin/qutebrowser "$@"
    '') name groups precedence 60;

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
      "liloimnbhkghhdhlamdjipkmadhpcjmn" # krebsgold
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
    ( createQuteUser "qb" [ "audio" ] 20 )
    ( createFirefoxUser "ff" [ "audio" ] 10 )
    ( createChromiumUser "cr" [ "audio" "video" ] 9 )
    ( createChromiumUser "gm" [ "video" "audio" ] 8 )
    ( createChromiumUser "wk" [ "audio" ] 0 )
    ( createChromiumUser "fb" [ "audio" ] 0 )
    ( createChromiumUser "com" [ "audio" ] 0 )
    ( createChromiumUser "fin" [] (-1) )
  ];
}
