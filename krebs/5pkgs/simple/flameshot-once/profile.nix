with import <stockholm/lib>;
{ config, pkgs }:
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
  };

  cfg = eval.config;

  eval = evalModules {
    modules = singleton {
      _file = toString ./config.nix;
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
            "COPY"
            "SAVE"
            "EXIT"
            "BLUR"
          ];
          type = types.listOf (types.enum (attrNames ButtonType));
        };
        disabledTrayIcon = mkOption {
          default = true;
          type = types.bool;
        };
        drawThickness = mkOption {
          default = 8;
          type = types.positive;
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
    "/Dharkael/flameshot.ini".text = ''
      [General]
      buttons=@Variant(\0\0\0\x7f\0\0\0\v${toQList "int" cfg.buttons})
      disabledTrayIcon=${toJSON cfg.disabledTrayIcon}
      drawThickness=${toJSON cfg.drawThickness}
      savePath=${toJSON cfg.savePath}
      showDesktopNotification=${toJSON cfg.showDesktopNotification}
      showHelp=${toJSON cfg.showHelp}
    '';
  };

in

  pkgs.writeDash "flameshot.profile" ''
    export FLAMESHOT_CAPTURE_PATH=${cfg.savePath}
    export XDG_CONFIG_HOME=${XDG_CONFIG_HOME}
  ''
