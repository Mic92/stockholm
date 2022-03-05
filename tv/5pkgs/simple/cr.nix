{ pkgs }:

pkgs.writeDashBin "cr" ''
  set -efu
  if test -n "''${XDG_RUNTIME_DIR-}"; then
    cache_dir=$XDG_RUNTIME_DIR/chromium-disk-cache
  else
    cache_dir=/tmp/chromium-disk-cache_$LOGNAME
  fi
  export LC_TIME=de_DE.utf8
  exec ${pkgs.chromium}/bin/chromium \
      --ssl-version-min=tls1 \
      --disk-cache-dir="$cache_dir" \
      --disk-cache-size=50000000 \
      "$@"
''
