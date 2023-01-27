{ pkgs
, variant ? "x220"
}:

let
  lib = import ./lib;
  font-size = arg: {
    program = "${pkgs.font-size-alacritty}/bin/font-size-alacritty";
    args = [arg];
  };
  configs.default = lib.recursiveUpdate variants.${variant} {
    bell.animation = "EaseOut";
    bell.duration = 50;
    bell.color = "#ff00ff";
    colors.cursor.cursor      = "#f042b0";
    colors.primary.background = "#202020";
    colors.primary.foreground = "#d0d7d0";
    colors.normal.black       = "#000000";
    colors.normal.red         = "#cd0000";
    colors.normal.green       = "#00cd00";
    colors.normal.yellow      = "#bc7004";
    colors.normal.blue        = "#4343be";
    colors.normal.magenta     = "#cb06cb";
    colors.normal.cyan        = "#04c9c9";
    colors.normal.white       = "#bebebe";
    colors.bright.black       = "#727272";
    colors.bright.red         = "#fb6262";
    colors.bright.green       = "#72fb72";
    colors.bright.yellow      = "#fbfb72";
    colors.bright.blue        = "#7272fb";
    colors.bright.magenta     = "#fb53fb";
    colors.bright.cyan        = "#72fbfb";
    colors.bright.white       = "#fbfbfb";
    draw_bold_text_with_bright_colors = true;
    hints.enabled = [
      {
        regex = "(ipfs:|ipns:|magnet:|mailto:|gemini:|gopher:|https:|http:|news:|file:|git:|ssh:|ftp:)[^\\u0000-\\u001F\\u007F-\\u009F<>\"\\s{-}\\^⟨⟩`]+";
        mouse.enabled = true;
        post_processing = true;
        action = "Select";
      }
    ];
    scrolling.multiplier = 8;
  };
  configs.root = lib.recursiveUpdate configs.default {
    colors.primary.background = "#230000";
    colors.primary.foreground = "#e0c0c0";
    colors.normal.black       = "#800000";
  };
  configs.fzmenu = lib.recursiveUpdate configs.default {
    colors.primary.background = "#2A172A";
    window.dimensions.columns = 70;
    window.dimensions.lines = 9;
  };
  variants.hidpi = {
    font.normal.family = "iosevka tv 1";
    font.bold.family = "iosevka tv 1";
    font.italic.family = "iosevka tv 1";
    font.bold_italic.family = "iosevka tv 1";
    font.size = 5;
    key_bindings = [
      { key = "Up";   mods = "Control";       action = "IncreaseFontSize"; }
      { key = "Down"; mods = "Control";       action = "DecreaseFontSize"; }
      { key = "Down"; mods = "Shift|Control"; action = "ResetFontSize"; }
    ];
  };
  variants.x220 = {
    font.normal.family = "Clean";
    font.bold.family = "Clean";
    font.bold.style = "Regular";
    font.size = 10;
    key_bindings = [
      { key = "Up";   mods = "Shift|Control"; command = font-size "=14"; }
      { key = "Up";   mods = "Control";       command = font-size "+1"; }
      { key = "Down"; mods = "Control";       command = font-size "-1"; }
      { key = "Down"; mods = "Shift|Control"; command = font-size "=0"; }
    ];
  };
  writeProfile = name: config: let
    config-file =
      assert lib.types.filename.check name;
      pkgs.writeJSON "alacritty-tv-${name}.json" config;
  in pkgs.writeText "alacritty-tv-${name}.profile" /* sh */ ''
    # Use home so Alacritty can find the configuration without arguments.
    # HOME will be reset once in Alacritty.
    HOME=$XDG_RUNTIME_DIR/Alacritty-${name}
    export HOME

    # Tell Alacritty via XDG_RUNTIME_DIR where to create sockets.
    # XDG_RUNTIME_DIR needs to be reset manually.
    export ALACRITTY_XDG_RUNTIME_DIR="$XDG_RUNTIME_DIR"
    export BASH_EXTRA_INIT=${pkgs.writeDash "alacritty-tv.cleanup.sh" ''
      XDG_RUNTIME_DIR=$ALACRITTY_XDG_RUNTIME_DIR
      unset ALACRITTY_XDG_RUNTIME_DIR
      unset BASH_EXTRA_INIT
    ''}
    export XDG_RUNTIME_DIR="$HOME"

    # Install stored configuration if it has changed.
    # This allows for both declarative updates and runtime modifications.
    # rust-xdg requires XDG_RUNTIME_DIR to be secure:
    # https://docs.rs/xdg/2.4.1/src/xdg/lib.rs.html#311
    ${pkgs.coreutils}/bin/mkdir -m 0700 -p "$HOME"
    ref=$(! test -e "$HOME"/ref || ${pkgs.coreutils}/bin/cat "$HOME"/ref)
    if test "$ref" != ${config-file}; then
      echo ${config-file} > "$HOME"/ref
      ${pkgs.coreutils}/bin/cp ${config-file} "$HOME"/.alacritty.yml
    fi
  '';
in

pkgs.symlinkJoin {
  name = "alacritty-tv";
  paths = [
    (pkgs.writeDashBin "alacritty" ''
      # usage:
      #   alacritty [--profile=PROFILE] [--singleton] [ARGS...]
      # where
      #   PROFILE one of ${lib.toJSON (lib.attrNames configs)}

      set -efu

      case ''${1-} in
      ${lib.concatMapStringsSep "\n" (name: /* sh */ ''
        --${lib.shell.escape name}|--profile=${lib.shell.escape name})
          shift
          profile=${writeProfile name configs.${name}}
          ;;
      '') (lib.attrNames configs)}
        *)
          profile=${writeProfile "default" configs.default}
          ;;
      esac


      case ''${1-} in
        --singleton)
          shift
          if ! ${pkgs.alacritty}/bin/alacritty msg create-window "$@"; then
            . "$profile"
            ${pkgs.alacritty}/bin/alacritty "$@" &
          fi
          ;;
        *)
          . "$profile"
          exec ${pkgs.alacritty}/bin/alacritty "$@"
          ;;
      esac
    '')
    pkgs.alacritty
  ];
}
