{ pkgs }:

let
  lib = import ./lib;
  font-size = arg: {
    program = "${pkgs.font-size-alacritty}/bin/font-size-alacritty";
    args = [arg];
  };
  config = {
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
    font.normal.family = "Clean";
    font.bold.family = "Clean";
    font.bold.style = "Regular";
    font.size = 10;
    hints.enabled = [
      {
        regex = "(ipfs:|ipns:|magnet:|mailto:|gemini:|gopher:|https:|http:|news:|file:|git:|ssh:|ftp:)[^\\u0000-\\u001F\\u007F-\\u009F<>\"\\s{-}\\^⟨⟩`]+";
        mouse.enabled = true;
        post_processing = true;
        action = "Select";
      }
    ];
    key_bindings = [
      { key = "Up";   mods = "Shift|Control"; command = font-size "=14"; }
      { key = "Up";   mods = "Control";       command = font-size "+1"; }
      { key = "Down"; mods = "Control";       command = font-size "-1"; }
      { key = "Down"; mods = "Shift|Control"; command = font-size "=0"; }
    ];
    scrolling.multiplier = 8;
  };
  config-file = pkgs.writeJSON "alacritty-tv.json" config;
  profile = pkgs.writeText "alacritty-tv.profile" /* sh */ ''
    # Use home so Alacritty can find the configuration without arguments.
    # HOME will be reset once in Alacritty.
    HOME=$TMPDIR/Alacritty
    export HOME

    # Install stored configuration if it has changed.
    # This allows for both declarative updates and runtime modifications.
    ${pkgs.coreutils}/bin/mkdir -p "$HOME"
    if test "$(${pkgs.coreutils}/bin/cat "$HOME"/ref)" != ${config-file}; then
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
      #   alacritty [--singleton] [ARGS...]

      set -efu

      case ''${1-} in
        --singleton)
          shift
          if ! ${pkgs.alacritty}/bin/alacritty msg create-window "$@"; then
            . ${profile}
            ${pkgs.alacritty}/bin/alacritty "$@" &
          fi
          ;;
        *)
          . ${profile}
          exec ${pkgs.alacritty}/bin/alacritty "$@"
          ;;
      esac
    '')
    pkgs.alacritty
  ];
}
