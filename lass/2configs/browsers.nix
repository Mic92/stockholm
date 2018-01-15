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

  createChromiumUser = name: extraGroups: precedence:
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
      lass.browser.paths.${name} = {
        path = bin;
        inherit precedence;
      };
      security.sudo.extraConfig = ''
        ${mainUser.name} ALL=(${name}) NOPASSWD: ALL
      '';
      environment.systemPackages = [
        bin
      ];
    };

  createFirefoxUser = name: extraGroups: precedence:
    let
      bin = pkgs.writeScriptBin name ''
        /var/run/wrappers/bin/sudo -u ${name} -i ${pkgs.firefox-devedition-bin}/bin/firefox-devedition $@
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
      lass.browser.paths.${name} = {
        path = bin;
        inherit precedence;
      };
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
    ( createChromiumUser "cr" [ "video" "audio" ] 9 )
    ( createChromiumUser "gm" [ "video" "audio" ] 8 )
    ( createChromiumUser "wk" [ "video" "audio" ] )
    ( createChromiumUser "fb" [ "video" "audio" ] )
    ( createChromiumUser "com" [ "video" "audio" ] )
  ];
}
