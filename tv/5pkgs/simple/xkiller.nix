{ pkgs }:
pkgs.writeDash "xkiller" ''
  set -efu
  exec >&2
  ${pkgs.iproute}/bin/ss -lp src unix:/tmp/.X11-unix/X* |
  ${pkgs.gnused}/bin/sed -n '
    s|.*/tmp/.X11-unix/X\([0-9]\+\)\>.*("X[^"]*",pid=\([0-9]\+\)\>.*|\1 \2|p
  ' |
  while read -r display pid; do
    {
      exit_code=$(
        DISPLAY=:$display ${pkgs.coreutils}/bin/timeout 1 \
            ${pkgs.xorg.xset}/bin/xset q >/dev/null 2>&1 &&
            echo 0 || echo $?
      )
      if test $exit_code = 124; then
        echo "X on display :$display is locked up; killing PID $pid..."
        ${pkgs.coreutils}/bin/kill -SIGKILL "$pid"
      else
        echo "X on display :$display is healthy"
      fi
    } &
  done
  wait
''
