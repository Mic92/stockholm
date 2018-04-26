{ writeDashBin }:
writeDashBin "font-size" ''
  set -efu

  # set_font NORMAL_FONT BOLD_FONT
  set_font() {
    printf '\033]710;%s\007' "$1"
    printf '\033]711;%s\007' "$2"
  }

  case ''${1-} in
    '''|0|--reset)
      set_font \
          -*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1 \
          -*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1 \
      ;;
    [1-9]|[1-9][0-9]|[1-9][0-9][0-9])
      set_font \
          xft:Monospace:size=$1 \
          xft:Monospace:size=$1:bold \
      ;;
    *)
      echo "$0: bad argument: $1" >&2
      exit 1
  esac
''
