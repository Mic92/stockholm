{ writeDashBin, xdotool, xorg }:
writeDashBin "xwaitforwindow" ''
  # usage: xwaitforwindow ARGS
  # see xdotool search for possible ARGS
  # example: xwaitforwindow -name WINDOWNAME
  set -efu

  if id=$(${xdotool}/bin/xdotool search "$@"); then
    printf 'waiting for window %#x\n' "$id" >&2
    exec ${xorg.xprop}/bin/xprop -spy -id "$id" >/dev/null
  else
    printf 'no window found with xdotool search %s\n' "$*" >&2
    exit 1
  fi
''
