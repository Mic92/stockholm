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
  PROMPT=">"
  for i in "$@"
  do
  case $i in
      -p)
      PROMPT="$2"
      shift
      shift
      break
      ;;
      -l)
      # no reason to filter number of lines
      LINES="$2"
      shift
      shift
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
  INPUT=$(${pkgs.coreutils}/bin/cat)
  OUTPUT="$(${pkgs.coreutils}/bin/mktemp)"
  if [ -z ''${TERM+x} ]; then #check if we can print fzf in the shell
    ${pkgs.rxvt_unicode}/bin/urxvt \
      -name ${cfg.appName} \
      -title ${shell.escape cfg.windowTitle} \
      -e ${pkgs.dash}/bin/dash -c \
        "echo \"$INPUT\" | ${pkgs.fzf}/bin/fzf \
          --history=/dev/null \
          --print-query \
          --prompt=\"$PROMPT\" \
          --reverse \
          > \"$OUTPUT\"" 2>/dev/null
  else
    echo "$INPUT" | ${pkgs.fzf}/bin/fzf \
      --history=/dev/null \
      --print-query \
      --prompt="$PROMPT" \
      --reverse \
      > "$OUTPUT"
  fi
  ${pkgs.coreutils}/bin/tail -1 "$OUTPUT"
  ${pkgs.coreutils}/bin/rm "$OUTPUT"
''
