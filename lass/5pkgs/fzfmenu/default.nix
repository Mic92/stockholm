{ pkgs, ... }:

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
  ${pkgs.rxvt_unicode}/bin/urxvt \
    -name fzfmenu -title fzfmenu \
    -e ${pkgs.dash}/bin/dash -c \
      "echo \"$INPUT\" | ${pkgs.fzf}/bin/fzf \
        --history=/dev/null \
        --print-query \
        --prompt=\"$PROMPT\" \
        > \"$OUTPUT\"" 2>/dev/null
  ${pkgs.coreutils}/bin/tail -1 "$OUTPUT"
  ${pkgs.coreutils}/bin/rm "$OUTPUT"
''
