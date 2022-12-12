{ pkgs }:

pkgs.writeDashBin "font-size-alacritty" ''
  # usage: font-size-alacritty (+N|-N|=N)
  # Increase by, decrease by, or set font size to the value N.

  set -efu

  min_size=8

  op=''${1%%[0-9]*}
  op=''${op:-=}

  value=''${1#[=+-]}

  window_id=$(${pkgs.xdotool}/bin/xdotool getactivewindow)

  current_size=$(
    ${pkgs.xorg.xprop}/bin/xprop -notype -id $window_id FONT_SIZE |
    ${pkgs.gnused}/bin/sed -rn 's/.* = ([0-9]+)$/\1/p'
  )

  # usage: set_font_size WINDOW_ID FONT_SIZE
  set_font_size() {
    ${pkgs.alacritty}/bin/alacritty msg config -w $1 font.size=$2
    ${pkgs.xorg.xprop}/bin/xprop -id $1 -f FONT_SIZE 32c -set FONT_SIZE $2
  }

  # usage: reset_font_size WINDOW_ID
  reset_font_size() {
    ${pkgs.alacritty}/bin/alacritty msg config -w $1 font.size=$min_size
    ${pkgs.xorg.xprop}/bin/xprop -id $1 -remove FONT_SIZE
  }

  # usage: make_next_size
  make_next_size() {
    case $op in
      -) next_size=$(expr $current_size - $value) ;;
      =) next_size=$value ;;
      +)
        next_size=$(expr $current_size + $value)
        test $next_size -ge $min_size || next_size=$min_size
        ;;
    esac
  }

  if test -z "$current_size"; then
    current_size=0
    make_next_size
    if test $next_size -ge $min_size; then
      ${pkgs.alacritty}/bin/alacritty msg config -w $window_id \
          font.normal.family='Input Mono' \
          font.normal.style=Condensed \
          font.bold.family='Input Mono' \
          font.bold.style=Bold
      set_font_size $window_id $next_size
    fi
  else
    make_next_size
    if test $next_size -ge $min_size; then
      set_font_size $window_id $next_size
    else
      ${pkgs.alacritty}/bin/alacritty msg config -w $window_id -r
      reset_font_size $window_id
    fi
  fi
''
