{ config, pkgs }:
with pkgs.stockholm.lib;
with generators;
let

  # Refs https://github.com/lupoDharkael/flameshot/blob/master/src/widgets/capture/capturebutton.h
  ButtonType = {
    PENCIL             = 0;
    DRAWER             = 1;
    ARROW              = 2;
    SELECTION          = 3;
    RECTANGLE          = 4;
    CIRCLE             = 5;
    MARKER             = 6;
    SELECTIONINDICATOR = 7;
    MOVESELECTION      = 8;
    UNDO               = 9;
    COPY               = 10;
    SAVE               = 11;
    EXIT               = 12;
    IMAGEUPLOADER      = 13;
    OPEN_APP           = 14;
    BLUR               = 15;
    REDO               = 16;
    PIN                = 17;
    TEXT               = 18;
    CIRCLECOUNT        = 19;
  };

  cfg = eval.config;

  eval = evalModules {
    modules = singleton {
      _file = toString ./profile.nix;
      imports = singleton config;
      options = {
        buttons = mkOption {
          apply = map (name: ButtonType.${name});
          default = [
            "PENCIL"
            "DRAWER"
            "ARROW"
            "SELECTION"
            "RECTANGLE"
            "CIRCLE"
            "MARKER"
            "SELECTIONINDICATOR"
            "MOVESELECTION"
            "UNDO"
            "SAVE"
            "EXIT"
            "BLUR"
            "CIRCLECOUNT"
          ]
          ++ optional cfg.imgur.enable "IMAGEUPLOADER"
          ;
          type = types.listOf (types.enum (attrNames ButtonType));
        };
        copyAndCloseAfterUpload = mkOption {
          default = false;
          type = types.bool;
        };
        disabledTrayIcon = mkOption {
          default = true;
          type = types.bool;
        };
        drawThickness = mkOption {
          default = 8;
          type = types.positive;
        };
        filenamePattern = mkOption {
          default = "%FT%T%z_flameshot";
          type =
            # This is types.filename extended by [%:][%:+]*
            types.addCheck types.str (test "[%:0-9A-Za-z._][%:+0-9A-Za-z._-]*");
        };
        imgur = mkOption {
          default = {};
          type = types.submodule {
            options = {
              enable = mkEnableOption "imgur";
              createUrl = mkOption {
                example = "http://p.r/image";
                type = types.str;
              };
              deleteUrl = mkOption {
                example = "http://p.r/image/delete/%1";
                type = types.str;
              };
              xdg-open = mkOption {
                default = {};
                type = types.submodule {
                  options = {
                    enable = mkEnableOption "imgur.xdg-open" // {
                      default = true;
                    };
                    browser = mkOption {
                      default = "${pkgs.coreutils}/bin/false";
                      type = types.str;
                    };
                    createPrefix = mkOption {
                      default = cfg.imgur.createUrl;
                      type = types.str;
                    };
                    deletePrefix = mkOption {
                      default = removeSuffix "/%1" cfg.imgur.deleteUrl;
                      type = types.str;
                    };
                  };
                };
              };
            };
          };
        };
        savePath = mkOption {
          default = "/tmp";
          type = types.absolute-pathname;
        };
        showDesktopNotification = mkOption {
          default = false;
          type = types.bool;
        };
        showHelp = mkOption {
          default = false;
          type = types.bool;
        };
        showSidePanelButton = mkOption {
          default = false;
          type = types.bool;
        };
        showStartupLaunchMessage = mkOption {
          default = false;
          type = types.bool;
        };
        timeout = mkOption {
          default = 200;
          description = ''
            Maximum time in milliseconds allowed for the flameshot daemon to
            react.
          '';
          type = types.positive;
        };
      };
    };
  };

  hexchars = stringToCharacters "0123456789abcdef";

  # Encode integer to C-escaped string of bytes, little endian / LSB 0
  le = rec {
    x1 = i: let
      i0 = mod i 16;
      i1 = i / 16;
    in
      "\\x${elemAt hexchars i1}${elemAt hexchars i0}";

    x2 = i: let
      i0 = mod i 256;
      i1 = i / 256;
    in
      "${x1 i0}${x1 i1}";

    x4 = i: let
      i0 = mod i 65536;
      i1 = i / 65536;
    in
      "${x2 i0}${x2 i1}";
  };

  toQList = t: xs:
    assert t == "int";
    "QList<${t}>${le.x4 0}${le.x4 (length xs)}${concatMapStrings le.x4 xs}";

  XDG_CONFIG_HOME = pkgs.write "flameshot-config" {
    "/flameshot/flameshot.ini".text =
      toINI {} {
        General = {
          buttons = ''@Variant(\0\0\0\x7f\0\0\0\v${toQList "int" cfg.buttons})'';
          disabledTrayIcon = cfg.disabledTrayIcon;
          checkForUpdates = false;
          copyAndCloseAfterUpload = cfg.copyAndCloseAfterUpload;
          drawThickness = cfg.drawThickness;
          filenamePattern = cfg.filenamePattern;
          savePath = cfg.savePath;
          showDesktopNotification = cfg.showDesktopNotification;
          showHelp = cfg.showHelp;
          showSidePanelButton = cfg.showSidePanelButton;
          showStartupLaunchMessage = cfg.showStartupLaunchMessage;
        };
        Shortcuts = {
          TYPE_COPY = "Return";
        };
      };
  };

in

  pkgs.writeDash "flameshot.profile" ''
    export FLAMESHOT_CAPTURE_PATH=${cfg.savePath}
    export FLAMESHOT_ONCE_TIMEOUT=${toString cfg.timeout}
    export XDG_CONFIG_HOME=${XDG_CONFIG_HOME}
    ${optionalString cfg.imgur.enable /* sh */ ''
      export IMGUR_CREATE_URL=${shell.escape cfg.imgur.createUrl}
      export IMGUR_DELETE_URL=${shell.escape cfg.imgur.deleteUrl}
      ${optionalString cfg.imgur.xdg-open.enable /* sh */ ''
        PATH=$PATH:${makeBinPath [
          (pkgs.writeDashBin "xdg-open" ''
            set -efu
            uri=$1
            prefix=$(${pkgs.coreutils}/bin/dirname "$uri")
            case $prefix in
              (${shell.escape cfg.imgur.xdg-open.createPrefix})
                echo "opening image in browser: $uri" >&2
                exec ${config.imgur.xdg-open.browser} "$uri"
                ;;
              (${shell.escape cfg.imgur.xdg-open.deletePrefix})
                echo "deleting image: $uri" >&2
                exec ${pkgs.curl}/bin/curl -fsS -X DELETE "$uri"
                ;;
              (*)
                echo "don't know how to open URI: $uri" >&2
                exit 1
            esac
          '')
        ]}
      ''}
    ''}
  ''
