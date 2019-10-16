{ config, lib, pkgs, ... }:
with import <stockholm/lib>;
let

  cfg = config.lass.browser;

  browserScripts = {
    chromium = "${pkgs.chromium}/bin/chromium";
    firefox = "${pkgs.firefox.override {
        extraNativeMessagingHosts = [ pkgs.tridactyl-native ];
      }}/bin/firefox";
    qutebrowser = "${pkgs.qutebrowser}/bin/qutebrowser";
  };

  browser-select = let
    sortedPaths = sort (a: b: a.value.precedence > b.value.precedence)
                       (mapAttrsToList (name: value: { inherit name value; })
                                       cfg.config);
  in if (lib.length sortedPaths) > 1 then
    pkgs.writeScriptBin "browser-select" ''
      BROWSER=$(echo -e "${concatStringsSep "\\n" (map (getAttr "name") sortedPaths)}" | ${pkgs.dmenu}/bin/dmenu)
      case $BROWSER in
      ${concatMapStringsSep "\n" (n: ''
        ${n.name})
          export BIN=${config.lass.xjail-bins.${n.name}}/bin/${n.name}
          ;;
      '') (sortedPaths)}
      esac
      $BIN "$@"
    ''
  else
    let
      name = (lib.head sortedPaths).name;
    in pkgs.writeScriptBin "browser-select" ''
      ${config.lass.xjail-bins.${name}}/bin/${name} "$@"
    ''
  ;

in {
  options.lass.browser = {
    select = mkOption {
      type = types.path;
    };
    config = mkOption {
      type = types.attrsOf (types.submodule ({ config, ... }: {
        options = {
          name = mkOption {
            type = types.str;
            default = config._module.args.name;
          };
          precedence = mkOption {
            type = types.int;
            default = 0;
          };
          user = mkOption {
            type = types.str;
            default = config._module.args.name;
          };
          browser = mkOption {
            type = types.enum (attrNames browserScripts);
            default = "chromium";
          };
          groups = mkOption {
            type = types.listOf types.str;
            default = [];
          };
        };
      }));
      default = {};
    };
  };

  config = (mkIf (cfg.config != {}) {
    lass.xjail = mapAttrs' (name: browser:
      nameValuePair name {
        script = browserScripts.${browser.browser};
        groups = browser.groups;
      }
    ) cfg.config;
    environment.systemPackages = (map (browser:
      config.lass.xjail-bins.${browser.name}
    ) (attrValues cfg.config))  ++ [
      browser-select
    ];
    lass.browser.select = browser-select;
  });
}
