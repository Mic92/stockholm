{ pkgs }:

pkgs.writeDashBin "ff" ''
  case $TOUCHSCREEN in 1)
    export MOZ_USE_XINPUT2=1
  esac
  exec ${pkgs.firefox}/bin/firefox "$@"
''
