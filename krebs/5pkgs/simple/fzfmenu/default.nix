with import <stockholm/lib>;
{ pkgs, ... }@args:

let
  # config cannot be declared in the input attribute set because that would
  # cause callPackage to inject the wrong config.  Instead, get it from ...
  # via args.
  config = args.config or {};

  cfg = eval.config;

  eval = evalModules {
    modules = singleton {
      _file = toString ./profile.nix;
      imports = singleton config;
      options = {
        defaultPrompt = mkOption {
          default = ">";
          type = types.str;
        };
        appName = mkOption {
          default = "fzfmenu";
          type = types.label;
        };
        windowTitle = mkOption {
          default = "fzfmenu";
          type = types.str;
        };
      };
    };
  };
in

pkgs.writeDashBin "fzfmenu" ''
  set -efu

  # Spawn terminal if called without one, like e.g. from a window manager.
  if [ -z ''${TERM+x} ]; then
    exec 3<&0
    exec 4>&1
    export FZFMENU_INPUT_FD=3
    export FZFMENU_OUTPUT_FD=4
    exec ${pkgs.rxvt_unicode}/bin/urxvt \
        -name ${cfg.appName} \
        -title ${shell.escape cfg.windowTitle} \
        -e "$0" "$@"
  else
    exec 0<&''${FZFMENU_INPUT_FD-0}
    exec 1>&''${FZFMENU_OUTPUT_FD-1}
  fi

  PROMPT=${shell.escape cfg.defaultPrompt}
  for i in "$@"; do
    case $i in
      -p)
        PROMPT=$2
        shift 2
        break
        ;;
      -l)
        # no reason to filter number of lines
        LINES=$2
        shift 2
        break
        ;;
      -i)
        # we do this anyway
        shift
        break
        ;;
      *)
        echo "Unknown option $1" >&2
        shift
        ;;
    esac
  done

  ${pkgs.fzf}/bin/fzf \
      --history=/dev/null \
      --print-query \
      --prompt="$PROMPT" \
      --reverse \
    |
  ${pkgs.coreutils}/bin/tail -1
''
