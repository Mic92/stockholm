{ config, pkgs, ... }:
with pkgs.stockholm.lib;

let
  # Encode integer to C-escaped string of bytes, little endian / LSB 0
  le = rec {
    x1 = i: let
      i0 = mod i 16;
      i1 = i / 16;
    in
      if i == 0 then
        "\\0"
      else if i < 16 then
        "\\x${elemAt hexchars i0}"
      else
        "\\x${elemAt hexchars i1}${elemAt hexchars i0}";

    x2 = i: let
      i0 = mod i 256;
      i1 = i / 256;
    in
      "${x1 i1}${x1 i0}";

    x4 = i: let
      i0 = mod i 65536;
      i1 = i / 65536;
    in
      "${x2 i1}${x2 i0}";
  };

  toQList = t: xs:
    assert t == "int";
    "QList<${t}>${le.x4 0}${le.x1 (length xs)}${concatMapStrings le.x4 xs}";
in

{
  options = {
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
                  default = config.imgur.createUrl;
                  type = types.str;
                };
                deletePrefix = mkOption {
                  default = removeSuffix "/%1" config.imgur.deleteUrl;
                  type = types.str;
                };
              };
            };
          };
        };
      };
    };
    package = mkOption {
      type = types.package;
      default = import ./flameshot { inherit pkgs; };
    };
    settings = {
      # Options without a description are not documented in flameshot's README.
      General = mapAttrs (_: recursiveUpdate { default = null; }) {
        allowMultipleGuiInstances = mkOption {
          description = ''
            Allow multiple instances of `flameshot gui` to run at the same time
          '';
          type = with types; nullOr bool;
        };
        antialiasingPinZoom = mkOption {
          description = ''
            Anti-aliasing image when zoom the pinned image
          '';
          type = with types; nullOr bool;
        };
        autoCloseIdleDaemon = mkOption {
          description = ''
            Automatically close daemon when it's not needed
          '';
          type = with types; nullOr bool;
        };
        buttons = let
          buttonTypes = {
            TYPE_PENCIL = 0;
            TYPE_DRAWER = 1;
            TYPE_ARROW = 2;
            TYPE_SELECTION = 3;
            TYPE_RECTANGLE = 4;
            TYPE_CIRCLE = 5;
            TYPE_MARKER = 6;
            TYPE_SELECTIONINDICATOR = 7;
            TYPE_MOVESELECTION = 8;
            TYPE_UNDO = 9;
            TYPE_COPY = 10;
            TYPE_SAVE = 11;
            TYPE_EXIT = 12;
            TYPE_IMAGEUPLOADER = 13;
            TYPE_OPEN_APP = 14;
            TYPE_PIXELATE = 15;
            TYPE_REDO = 16;
            TYPE_PIN = 17;
            TYPE_TEXT = 18;
            TYPE_CIRCLECOUNT = 19;
            TYPE_SIZEINCREASE = 20;
            TYPE_SIZEDECREASE = 21;
            TYPE_INVERT = 22;
            TYPE_ACCEPT = 23;
          };
          iterableButtonTypes = [
            "TYPE_ACCEPT"
            "TYPE_ARROW"
            "TYPE_CIRCLE"
            "TYPE_CIRCLECOUNT"
            "TYPE_COPY"
            "TYPE_DRAWER"
            "TYPE_EXIT"
            "TYPE_IMAGEUPLOADER"
            "TYPE_MARKER"
            "TYPE_MOVESELECTION"
            "TYPE_OPEN_APP"
            "TYPE_PENCIL"
            "TYPE_PIN"
            "TYPE_PIXELATE"
            "TYPE_RECTANGLE"
            "TYPE_REDO"
            "TYPE_SAVE"
            "TYPE_SELECTION"
            "TYPE_SIZEDECREASE"
            "TYPE_SIZEINCREASE"
            "TYPE_TEXT"
            "TYPE_UNDO"
          ];
        in mkOption {
          apply = names:
            if names != null then let
              values = map (name: buttonTypes.${name}) names;
            in
              ''@Variant(\0\0\0\x7f\0\0\0\v${toQList "int" values})''
            else
              null;
          description = ''
            Configure which buttons to show after drawing a selection
          '';
          type = with types; nullOr (listOf (enum iterableButtonTypes));
        };
        checkForUpdates = mkOption {
          type = with types; nullOr bool;
        };
        contrastOpacity = mkOption {
          description = ''
            Opacity of area outside selection
          '';
          type = with types; nullOr (boundedInt 0 255);
        };
        contrastUiColor = mkOption {
          description = ''
            Contrast UI color
          '';
          type = with types; nullOr flameshot.color;
        };
        copyAndCloseAfterUpload = mkOption {
          type = with types; nullOr bool;
        };
        copyOnDoubleClick = mkOption {
          type = with types; nullOr bool;
        };
        copyPathAfterSave = mkOption {
          description = ''
            Copy path to image after save
          '';
          type = with types; nullOr bool;
        };
        copyURLAfterUpload = mkOption {
          description = ''
            On successful upload, close the dialog and copy URL to clipboard
          '';
          type = with types; nullOr bool;
        };
        disabledTrayIcon = mkOption {
          description = ''
            Whether the tray icon is disabled
          '';
          type = with types; nullOr bool;
        };
        drawColor = mkOption {
          description = ''
            Last used color
          '';
          type = with types; nullOr flameshot.color;
        };
        drawFontSize = mkOption {
          type = with types; nullOr positive;
        };
        drawThickness = mkOption {
          description = ''
            Last used tool thickness
          '';
          type = with types; nullOr positive;
        };
        filenamePattern = mkOption {
          description = ''
            Filename pattern using C++ strftime formatting
          '';
          type =
            # This is types.filename extended by [%:][%:+]*
            with types;
            nullOr (addCheck str (test "[%:0-9A-Za-z._][%:+0-9A-Za-z._-]*"));
        };
        fontFamily = mkOption {
          type = with types; nullOr str;
        };
        historyConfirmationToDelete = mkOption {
          type = with types; nullOr bool;
        };
        ignoreUpdateToVersion = mkOption {
          description = ''
            Ignore updates to versions less than this value
          '';
          type = with types; nullOr str;
        };
        keepOpenAppLauncher = mkOption {
          description = ''
            Keep the App Launcher open after selecting an app
          '';
          type = with types; nullOr bool;
        };
        predefinedColorPaletteLarge = mkOption {
          description = ''
            Use larger color palette as the default one
          '';
          type = with types; nullOr bool;
        };
        saveAfterCopy = mkOption {
          description = ''
            Save image after copy
          '';
          type = with types; nullOr bool;
        };
        saveAsFileExtension = mkOption {
          description = ''
            Default file extension for screenshots
          '';
          type = with types; nullOr (addCheck filename (hasPrefix "."));
        };
        safeLastRegion = mkOption {
          type = with types; nullOr bool;
        };
        savePath = mkOption {
          description = ''
            Image Save Path
          '';
          type = with types; nullOr absolute-pathname;
        };
        savePathFixed = mkOption {
          description = ''
            Whether the savePath is a fixed path
          '';
          type = with types; nullOr bool;
        };
        showDesktopNotification = mkOption {
          description = ''
            Show desktop notifications
          '';
          type = with types; nullOr bool;
        };
        showHelp = mkOption {
          description = ''
            Show the help screen on startup
          '';
          type = with types; nullOr bool;
        };
        showMagnifier = mkOption {
          type = with types; nullOr bool;
        };
        showSelectionGeometry = mkOption {
          type = with types; nullOr (boundedInt 0 5);
        };
        showSelectionGeometryHideTime = mkOption {
          type = with types; nullOr uint;
        };
        showSidePanelButton = mkOption {
          description = ''
            Show the side panel button
          '';
          type = with types; nullOr bool;
        };
        showStartupLaunchMessage = mkOption {
          type = with types; nullOr bool;
        };
        squareMagnifier = mkOption {
          type = with types; nullOr bool;
        };
        startupLaunch = mkOption {
          description = ''
            Launch at startup
          '';
          type = with types; nullOr bool;
        };
        uiColor = mkOption {
          description = ''
            Main UI color
          '';
          type = with types; nullOr flameshot.color;
        };
        undoLimit = mkOption {
          type = with types; nullOr (boundedInt 0 999);
        };
        uploadClientSecret = mkOption {
          type = with types; nullOr str;
        };
        uploadHistoryMax = mkOption {
          type = with types; nullOr uint;
        };
        uploadWithoutConfirmation = mkOption {
          description = ''
            Upload to imgur without confirmation
          '';
          type = with types; nullOr bool;
        };
        useJpgForClipboard = mkOption {
          description = ''
            Use JPG format instead of PNG
          '';
          type = with types; nullOr bool;
        };
        userColors = mkOption {
          apply = value:
            if value != null then
              concatStringsSep ", " value
            else
              null;
          description = ''
            List of colors for color picker
            The colors are arranged counter-clockwise with the first being set
            to the right of the cursor.  "picker" adds a custom color picker.
          '';
          type =
            with types;
            nullOr (listOf (either flameshot.color (enum ["picker"])));
        };
      };
      Shortcuts = genAttrs [
        "TYPE_ACCEPT"
        "TYPE_ARROW"
        "TYPE_CIRCLE"
        "TYPE_CIRCLECOUNT"
        "TYPE_COMMIT_CURRENT_TOOL"
        "TYPE_COPY"
        "TYPE_DELETE_CURRENT_TOOL"
        "TYPE_DRAWER"
        "TYPE_EXIT"
        "TYPE_IMAGEUPLOADER"
        "TYPE_INVERT"
        "TYPE_MARKER"
        "TYPE_MOVESELECTION"
        "TYPE_MOVE_DOWN"
        "TYPE_MOVE_LEFT"
        "TYPE_MOVE_RIGHT"
        "TYPE_MOVE_UP"
        "TYPE_OPEN_APP"
        "TYPE_PENCIL"
        "TYPE_PIN"
        "TYPE_PIXELATE"
        "TYPE_RECTANGLE"
        "TYPE_REDO"
        "TYPE_RESIZE_DOWN"
        "TYPE_RESIZE_LEFT"
        "TYPE_RESIZE_RIGHT"
        "TYPE_RESIZE_UP"
        "TYPE_SAVE"
        "TYPE_SELECTION"
        "TYPE_SELECTIONINDICATOR"
        "TYPE_SELECT_ALL"
        "TYPE_SIZEDECREASE"
        "TYPE_SIZEINCREASE"
        "TYPE_SYM_RESIZE_DOWN"
        "TYPE_SYM_RESIZE_LEFT"
        "TYPE_SYM_RESIZE_RIGHT"
        "TYPE_SYM_RESIZE_UP"
        "TYPE_TEXT"
        "TYPE_TOGGLE_PANEL"
        "TYPE_UNDO"
      ] (name: mkOption {
        default = null;
        type = with types; nullOr str;
      });
    };
  };
}
